# ANOVA of total biomass
# Auteur : Yansong Huang
# Date de création : 2024-10-31

library(tidyr)
library(dplyr)
library(purrr)
library(car)

# variables globales
deployment_scenarios <- c("cout","protection","loin","equilibre")
regulation_scenarios <- c("sans_fermeture","fermeture_chalut","fermeture_totale")
CC_scenarios <- c("ON","OFF")
n_years_cut <- c(10,21,22,34,35,49)
n_replicate <- 30

process_biomass <- function(current_results_path,cut_off_year_begin, cut_off_year_end) {
  list_biomass_base <- list.files(results_path_base, "Yansong_biomass_Simu.*csv", full.names = TRUE)
  list_biomass_current <- list.files(current_results_path, "Yansong_biomass_Simu.*csv", full.names = TRUE)
  
  biomass_relative <- lapply(1:n_replicate, function(simulation) {
    biomass_brut_base <- read.csv(list_biomass_base[simulation], skip = 1)
    biomass_brut_current <- read.csv(list_biomass_current[simulation], skip = 1)
    biomass_total_base <- biomass_brut_base %>% 
      filter(Time>cut_off_year_begin)%>%
      filter(Time<cut_off_year_end)%>% 
      colMeans() %>% sum()
    biomass_total_current <- biomass_brut_current %>% 
      filter(Time>cut_off_year_begin)%>%
      filter(Time<cut_off_year_end)%>% 
      colMeans() %>% sum()
    biomass_ratio <- biomass_total_current/biomass_total_base
  })
  biomass_relative <- as.vector(biomass_relative) %>% as.numeric()
  return(biomass_relative)
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
  results_path_base <- file.path("outputs/results_1111", "Base_simu", "Base", "output", "CIEM")
  
  # 分别计算三个时间段的数据
  
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
colnames(total_biomass_all) <- c("biomass_ratio", "deployment","regulation")

total_biomass_all$regulation <- factor(
  total_biomass_all$regulation,
  levels = c("sans_fermeture", "fermeture_chalut", "fermeture_totale"),
  labels = c("no closure", "trawlers closure", "complete closure")
)

# Levene test 
leveneTest(biomass_ratio ~ regulation * deployment, data = total_biomass_all)

#ANOVa
anova_model <- aov(biomass_ratio ~ regulation * deployment, data = total_biomass_all)
summary(anova_model)

shapiro.test(residuals(anova_model))
# LM
lm <- lm(biomass_ratio ~ regulation * deployment, data = total_biomass_all)
summary(lm)
