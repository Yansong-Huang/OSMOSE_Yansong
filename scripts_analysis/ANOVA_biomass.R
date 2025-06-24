# ----------------------------------------------
# 本脚本执行基于多个风电场部署与渔业监管情境的总生物量变异性分析，
# 包括方差齐性检验（Levene）、方差分析（ANOVA）和事后 t 检验。
# This script performs an analysis of total biomass variability across 
# multiple deployment and regulation scenarios. It includes Levene's test 
# for variance homogeneity, ANOVA, and post-hoc t-tests.
# ----------------------------------------------

# ANOVA of total biomass
# Author: Yansong Huang
# Created on: 2024-10-31

library(tidyr)
library(dplyr)
library(purrr)
library(car)

# 全局变量定义 / Global variables
deployment_scenarios <- c("cout","protection","loin","equilibre")
regulation_scenarios <- c("sans_fermeture","fermeture_chalut","fermeture_totale")
CC_scenarios <- c("ON","OFF")
n_years_cut <- c(10,21,22,34,35,49)
n_replicate <- 30

# 计算相对生物量的函数
# Function to compute relative biomass
process_biomass <- function(current_results_path, cut_off_year_begin, cut_off_year_end) {
  list_biomass_base <- list.files(results_path_base, "Yansong_biomass_Simu.*csv", full.names = TRUE)
  list_biomass_current <- list.files(current_results_path, "Yansong_biomass_Simu.*csv", full.names = TRUE)
  
  biomass_relative <- lapply(1:n_replicate, function(simulation) {
    biomass_brut_base <- read.csv(list_biomass_base[simulation], skip = 1)
    biomass_brut_current <- read.csv(list_biomass_current[simulation], skip = 1)
    
    biomass_total_base <- biomass_brut_base %>% 
      filter(Time > cut_off_year_begin, Time < cut_off_year_end) %>%
      colMeans() %>% sum()
    
    biomass_total_current <- biomass_brut_current %>% 
      filter(Time > cut_off_year_begin, Time < cut_off_year_end) %>%
      colMeans() %>% sum()
    
    biomass_ratio <- biomass_total_current / biomass_total_base
  })
  
  biomass_relative <- as.vector(biomass_relative) %>% as.numeric()
  return(biomass_relative)
}

# 初始化数据框
# Initialize global dataframe
total_biomass_all <- data.frame()

# 遍历所有捕鱼监管情境
# Loop through all fishing regulation scenarios
for (regulation in regulation_scenarios) {
  
  # 构建结果路径
  # Build result paths
  results_path_1 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[1], "_", regulation), "Base", "output", "CIEM")
  results_path_2 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[2], "_", regulation), "Base", "output", "CIEM")
  results_path_3 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[3], "_", regulation), "Base", "output", "CIEM")
  results_path_4 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[4], "_", regulation), "Base", "output", "CIEM")
  
  results_path_scenario <- list(results_path_1, results_path_2, results_path_3, results_path_4)
  results_path_base <- file.path("outputs/results_1111", "Base_simu", "Base", "output", "CIEM")
  
  # 计算特定时间段的总生物量（如风电运营期）
  # Calculate total biomass for the specified period (e.g. wind farm operational period)
  total_biomass_period_list <- map(results_path_scenario, ~ process_biomass(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[5],
    cut_off_year_end = n_years_cut[6]
  ))
  
  names(total_biomass_period_list) <- c("cost", "protection", "distance", "balance")
  
  # 整理为表格形式并标注当前情境
  # Format into table and tag with current scenario
  total_biomass_period_table <- stack(total_biomass_period_list) %>%
    mutate(regulation = regulation)
  
  # 合并结果
  # Append results
  total_biomass_all <- rbind(total_biomass_all, total_biomass_period_table)
}

# 重命名列
# Rename columns
colnames(total_biomass_all) <- c("biomass_ratio", "deployment", "regulation")

# 翻译监管情境名称
# Translate regulation scenario labels
total_biomass_all$regulation <- factor(
  total_biomass_all$regulation,
  levels = c("sans_fermeture", "fermeture_chalut", "fermeture_totale"),
  labels = c("no closure", "trawlers closure", "complete closure")
)

# 方差齐性检验
# Levene's test for homogeneity of variance
leveneTest(biomass_ratio ~ regulation * deployment, data = total_biomass_all)

# 方差分析
# ANOVA
anova_model <- aov(biomass_ratio ~ regulation * deployment, data = total_biomass_all)
summary(anova_model)

# 残差正态性检验
# Shapiro-Wilk test for residual normality
shapiro.test(residuals(anova_model))

# 线性模型拟合（重复 ANOVA 过程）
# Linear model fit (same structure as ANOVA)
lm <- lm(biomass_ratio ~ regulation * deployment, data = total_biomass_all)
summary(lm)

# 示例：计算某特定情境的平均相对生物量
# Example: compute mean biomass ratio for specific scenario
total_biomass_all %>% 
  filter(deployment == "cost", regulation == "no closure") %>%
  select("biomass_ratio") %>%
  unlist() %>%
  mean()

# 筛选不同子集进行 t 检验
# Subset selection for t-tests
cost_no_closure <- total_biomass_all %>% 
  filter(deployment == "cost", regulation == "no closure") %>%
  pull(biomass_ratio)

distance_no_closure <- total_biomass_all %>% 
  filter(deployment == "distance", regulation == "no closure") %>%
  pull(biomass_ratio)

cost_trawlers_closure <- total_biomass_all %>% 
  filter(deployment == "cost", regulation == "trawlers closure") %>%
  pull(biomass_ratio)

protection_trawlers_closure <- total_biomass_all %>% 
  filter(deployment == "protection", regulation == "trawlers closure") %>%
  pull(biomass_ratio)

# t 检验
# t-tests
t.test(cost_no_closure, distance_no_closure, alternative = "less")
t.test(cost_trawlers_closure, protection_trawlers_closure, alternative = "greater")
