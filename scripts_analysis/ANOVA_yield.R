# ----------------------------------------------
# 本脚本分析“风电部署与渔业管控情境”对渔获总量（yield）的影响。
# 通过模拟结果计算相对渔获量变化（与基准情境比较），并进行方差分析（ANOVA）。
# This script evaluates how offshore wind deployment and fisheries regulation
# scenarios affect total fishery yield. It computes the relative yield (vs. baseline)
# and performs ANOVA and pairwise comparisons to assess scenario effects.
# ----------------------------------------------

# Boxplot of total yield
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

# 处理每个情境的渔获数据 / Function to process yield data
process_yield <- function(current_results_path, cut_off_year_begin, cut_off_year_end) {
  list_yield_base <- list.files(results_path_base, "Yansong_yield_Simu.*csv", full.names = TRUE)
  list_yield_current <- list.files(current_results_path, "Yansong_yield_Simu.*csv", full.names = TRUE)
  
  yield_relative <- lapply(1:n_replicate, function(simulation) {
    yield_brut_base <- read.csv(list_yield_base[simulation], skip = 1)
    yield_brut_current <- read.csv(list_yield_current[simulation], skip = 1)
    
    yield_total_base <- yield_brut_base %>%
      filter(Time > cut_off_year_begin, Time < cut_off_year_end) %>%
      colMeans() %>%
      sum()
    
    yield_total_current <- yield_brut_current %>%
      filter(Time > cut_off_year_begin, Time < cut_off_year_end) %>%
      colMeans() %>%
      sum()
    
    yield_ratio <- yield_total_current / yield_total_base
    return(yield_ratio)
  })
  
  yield_relative <- as.numeric(yield_relative)
  return(yield_relative)
}

# 初始化全局数据框 / Initialize global table
total_yield_all <- data.frame()

# 遍历所有渔业管控情境 / Loop through regulation scenarios
for (regulation in regulation_scenarios) {
  
  # 构建不同部署情境下的路径 / Build paths for each deployment scenario
  results_path_scenario <- lapply(deployment_scenarios, function(deployment) {
    file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment, "_", regulation), "Base", "output", "CIEM")
  })
  
  results_path_base <- file.path("outputs/results_1111", "Base_simu", "Base", "output", "CIEM")
  
  # 计算当前管控情境下，不同部署情境的相对渔获量
  # Compute yield ratios under each deployment scenario
  total_yield_period_list <- map(results_path_scenario, ~ process_yield(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[5],
    cut_off_year_end = n_years_cut[6]
  ))
  
  names(total_yield_period_list) <- c("cost", "protection", "distance", "balance")
  
  # 转换为数据框并添加当前管控情境标签 / Create data frame with regulation tag
  total_yield_period_table <- stack(total_yield_period_list) %>%
    mutate(regulation = regulation)
  
  total_yield_all <- rbind(total_yield_all, total_yield_period_table)
}

# 整理列名 / Rename columns
colnames(total_yield_all) <- c("yield_ratio", "deployment", "regulation")

# 因子化变量并添加标签 / Factorize and label regulation
total_yield_all$regulation <- factor(
  total_yield_all$regulation,
  levels = c("sans_fermeture", "fermeture_chalut", "fermeture_totale"),
  labels = c("no closure", "trawlers closure", "complete closure")
)

# 方差齐性检验 / Levene's test
leveneTest(yield_ratio ~ regulation * deployment, data = total_yield_all)

# 方差分析 / ANOVA
anova_model <- aov(yield_ratio ~ regulation * deployment, data = total_yield_all)
summary(anova_model)

# 正态性检验 / Normality test
shapiro.test(residuals(anova_model))

# 计算特定情境的平均值 / Mean yield for selected scenario
total_yield_all %>%
  filter(deployment == "protection", regulation == "no closure") %>%
  select("yield_ratio") %>%
  unlist() %>%
  mean()

# 情境组合对比：拖网渔具限制下的部署策略
# Pairwise t-tests under trawlers closure
cost_trawlers <- total_yield_all %>%
  filter(deployment == "cost", regulation == "trawlers closure") %>%
  pull(yield_ratio)

protection_trawlers <- total_yield_all %>%
  filter(deployment == "protection", regulation == "trawlers closure") %>%
  pull(yield_ratio)

distance_trawlers <- total_yield_all %>%
  filter(deployment == "distance", regulation == "trawlers closure") %>%
  pull(yield_ratio)

balance_trawlers <- total_yield_all %>%
  filter(deployment == "balance", regulation == "trawlers closure") %>%
  pull(yield_ratio)

t.test(cost_trawlers, protection_trawlers, alternative = "less")
t.test(cost_trawlers, distance_trawlers, alternative = "less")
t.test(cost_trawlers, balance_trawlers, alternative = "less")

# 情境组合对比：完全禁渔下的部署策略
# Pairwise t-tests under complete closure
cost_complete <- total_yield_all %>%
  filter(deployment == "cost", regulation == "complete closure") %>%
  pull(yield_ratio)

protection_complete <- total_yield_all %>%
  filter(deployment == "protection", regulation == "complete closure") %>%
  pull(yield_ratio)

distance_complete <- total_yield_all %>%
  filter(deployment == "distance", regulation == "complete closure") %>%
  pull(yield_ratio)

balance_complete <- total_yield_all %>%
  filter(deployment == "balance", regulation == "complete closure") %>%
  pull(yield_ratio)

t.test(cost_complete, protection_complete, alternative = "less")
t.test(cost_complete, distance_complete, alternative = "less")
t.test(cost_complete, balance_complete, alternative = "less")
