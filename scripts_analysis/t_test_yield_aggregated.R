# t test of total yield
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

process_yield <- function(current_results_path,cut_off_year_begin, cut_off_year_end) {
  list_yield_current <- list.files(current_results_path, "Yansong_yield_Simu.*csv", full.names = TRUE)
  
  yield_current_list <- lapply(1:n_replicate, function(simulation) {
    yield_brut_current <- read.csv(list_yield_current[simulation], skip = 1)
    yield_total_current <- yield_brut_current %>% 
      filter(Time>cut_off_year_begin)%>%
      filter(Time<cut_off_year_end)%>% 
      colMeans() %>% sum()
  })
  yield_current <- as.vector(yield_current_list) %>% as.numeric()
  return(yield_current)
}

# 初始化全局数据框
total_yield_all <- data.frame()

# 遍历每个捕鱼政策情境
for (regulation in regulation_scenarios) {
  
  # 构建场景路径
  results_path_1 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[1], "_", regulation), "Base", "output", "CIEM")
  results_path_2 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[2], "_", regulation), "Base", "output", "CIEM")
  results_path_3 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[3], "_", regulation), "Base", "output", "CIEM")
  results_path_4 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[4], "_", regulation), "Base", "output", "CIEM")
  
  results_path_scenario <- list(results_path_1, results_path_2, results_path_3, results_path_4)
  
  # 计算时间段的数据
  total_yield_period_list <- map(results_path_scenario, ~ process_yield(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[3],
    cut_off_year_end = n_years_cut[4]
  ))
  
  names(total_yield_period_list) <- c("cost", "protection", "distance", "balance")
  
  # 转换为数据框并添加标识
  total_yield_period_table <- stack(total_yield_period_list) %>%
    mutate(regulation = regulation)
  
  # 合并到全局数据框
  total_yield_all <- rbind(
    total_yield_all,
    total_yield_period_table
  )
}


colnames(total_yield_all) <- c("yield", "deployment","regulation")

total_yield_all$regulation <- factor(
  total_yield_all$regulation,
  levels = c("sans_fermeture", "fermeture_chalut", "fermeture_totale"),
  labels = c("no closure", "trawlers closure", "complete closure")
)

# treat reference simulations
total_yield_base_list <- process_yield(
  current_results_path = results_path_base,
  cut_off_year_begin = n_years_cut[5],
  cut_off_year_end = n_years_cut[6]
)
total_yield_base <- as.vector(total_yield_base_list)

total_yield_base_sample <- sample(total_yield_base, 90, replace = TRUE)
total_yield_base_sample_2 <- sample(total_yield_base, 120, replace = TRUE)
# test each deployment scenario
t.test(total_yield_base_sample,
       total_yield_all[total_yield_all$deployment=="cost",]$yield)
t.test(total_yield_base_sample,
       total_yield_all[total_yield_all$deployment=="protection",]$yield)
t.test(total_yield_base_sample,
       total_yield_all[total_yield_all$deployment=="distance",]$yield)
t.test(total_yield_base_sample,
       total_yield_all[total_yield_all$deployment=="balance",]$yield)
# test each regulation scenario
t.test(total_yield_base_sample_2,
       total_yield_all[total_yield_all$regulation=="no closure",]$yield)
t.test(total_yield_base_sample_2,
       total_yield_all[total_yield_all$regulation=="trawlers closure",]$yield)
t.test(total_yield_base_sample_2,
       total_yield_all[total_yield_all$regulation=="complete closure",]$yield)






