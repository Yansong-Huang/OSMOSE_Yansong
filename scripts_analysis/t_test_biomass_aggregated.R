# t test of total biomass
# Auteur : Yansong Huang
# Date de création : 2024-11-22

library(tidyr)
library(dplyr)
library(purrr)

# variables globales
deployment_scenarios <- c("cout","protection","loin","equilibre")
regulation_scenarios <- c("sans_fermeture","fermeture_chalut","fermeture_totale")
CC_scenarios <- c("ON","OFF")
n_years_cut <- c(10,21,22,34,35,49)
n_replicate <- 30
results_path_base <- file.path("outputs/results_1111", "Base_simu", "Base", "output", "CIEM")

process_biomass <- function(current_results_path,cut_off_year_begin, cut_off_year_end) {
  list_biomass_current <- list.files(current_results_path, "Yansong_biomass_Simu.*csv", full.names = TRUE)
  
  biomass_current_list <- lapply(1:n_replicate, function(simulation) {
    biomass_brut_current <- read.csv(list_biomass_current[simulation], skip = 1)
    biomass_total_current <- biomass_brut_current %>% 
      filter(Time>cut_off_year_begin)%>%
      filter(Time<cut_off_year_end)%>% 
      colMeans() %>% sum()
  })
  biomass_current <- as.vector(biomass_current_list) %>% as.numeric()
  return(biomass_current)
}

# 初始化全局数据框
total_biomass_all <- data.frame()

# 遍历每个捕鱼政策情境
for (regulation in regulation_scenarios) {
  
  # 构建场景路径
  results_path_1 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[1], "_", regulation), "Base", "output", "CIEM")
  results_path_2 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[2], "_", regulation), "Base", "output", "CIEM")
  results_path_3 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[3], "_", regulation), "Base", "output", "CIEM")
  results_path_4 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[4], "_", regulation), "Base", "output", "CIEM")
  
  results_path_scenario <- list(results_path_1, results_path_2, results_path_3, results_path_4)
  
  # 计算时间段的数据
  total_biomass_period_list <- map(results_path_scenario, ~ process_biomass(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[3],
    cut_off_year_end = n_years_cut[4]
  ))
  
  names(total_biomass_period_list) <- c("cost", "protection", "distance", "balance")
  
  # 转换为数据框并添加标识
  total_biomass_period_table <- stack(total_biomass_period_list) %>%
    mutate(regulation = regulation)
  
  # 合并到全局数据框
  total_biomass_all <- rbind(
    total_biomass_all,
    total_biomass_period_table
  )
}


colnames(total_biomass_all) <- c("biomass", "deployment","regulation")

total_biomass_all$regulation <- factor(
  total_biomass_all$regulation,
  levels = c("sans_fermeture", "fermeture_chalut", "fermeture_totale"),
  labels = c("no closure", "trawlers closure", "complete closure")
)

# treat reference simulations
total_biomass_base_list <- process_biomass(
  current_results_path = results_path_base,
  cut_off_year_begin = n_years_cut[5],
  cut_off_year_end = n_years_cut[6]
)
total_biomass_base <- as.vector(total_biomass_base_list)

total_biomass_base_sample <- sample(total_biomass_base, 90, replace = TRUE)
total_biomass_base_sample_2 <- sample(total_biomass_base, 120, replace = TRUE)
# test each deployment scenario
t.test(total_biomass_base_sample,
       total_biomass_all[total_biomass_all$deployment=="cost",]$biomass)
t.test(total_biomass_base_sample,
       total_biomass_all[total_biomass_all$deployment=="protection",]$biomass)
t.test(total_biomass_base_sample,
       total_biomass_all[total_biomass_all$deployment=="distance",]$biomass)
t.test(total_biomass_base_sample,
       total_biomass_all[total_biomass_all$deployment=="balance",]$biomass)
# test each regulation scenario
t.test(total_biomass_base_sample_2,
       total_biomass_all[total_biomass_all$regulation=="no closure",]$biomass)
t.test(total_biomass_base_sample_2,
       total_biomass_all[total_biomass_all$regulation=="trawlers closure",]$biomass)
t.test(total_biomass_base_sample_2,
       total_biomass_all[total_biomass_all$regulation=="complete closure",]$biomass)






