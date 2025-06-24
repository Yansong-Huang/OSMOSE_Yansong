# ----------------------------------------------
# 本脚本评估不同风电部署和渔业管控情境下，LFI（体长 ≥ 40 cm 的个体在总渔获中的比例）
# 的变化。包括 Levene 方差齐性检验、ANOVA、Shapiro 正态性检验、Kruskal 非参数检验。
# This script evaluates the Large Fish Indicator (LFI ≥ 40 cm) variation across 
# deployment and regulation scenarios. It includes Levene’s test, ANOVA, 
# Shapiro normality test, and Kruskal-Wallis non-parametric tests.
# ----------------------------------------------

# ANOVA of LFI catch ≥ 40 cm
# Author: Yansong Huang
# Created on: 2024-11-21

library(tidyr)
library(dplyr)
library(purrr)
library(ncdf4)
library(patchwork)
library(car)

# 全局变量 / Global variables
deployment_scenarios <- c("cout","protection","loin","equilibre")
regulation_scenarios <- c("sans_fermeture","fermeture_chalut","fermeture_totale")
n_years_cut <- c(10,21,22,34,35,49)
n_replicate <- 30

# LFI 指标计算函数
# Function to compute Large Fish Indicator (LFI)
LFI <- function(data_yield_size) {
  data_yield_size_40 <- data_yield_size %>%
    filter(Size >= 40)
  
  # 按时间汇总所有物种和体型的生物量总和
  # Sum biomass across all species and sizes per time step
  total_yield <- data_yield_size %>%
    group_by(Time) %>%
    summarize(total_yield = sum(across(2:17)))
  
  large_yield <- data_yield_size_40 %>%
    group_by(Time) %>%
    summarize(large_yield = sum(across(2:17)))
  
  # 计算时间序列上的 LFI
  # Compute time series of LFI
  lfi_series = large_yield$large_yield / total_yield$total_yield
  
  # 计算平均 LFI
  # Compute average LFI
  lfi_mean = mean(lfi_series, na.rm = TRUE)
  
  return(lfi_mean)
}

# LFI 主流程函数
# Main LFI processing function
process_LFI <- function(current_results_path, cut_off_year_begin, cut_off_year_end) {
  list_yield_base <- list.files(results_path_base, "Yansong_yieldDistribBySize_Simu.*csv", full.names = TRUE)
  list_yield_current <- list.files(current_results_path, "Yansong_yieldDistribBySize_Simu.*csv", full.names = TRUE)
  
  lfi_relative <- lapply(1:n_replicate, function(simulation) {
    yield_brut_base <- read.csv(list_yield_base[simulation], skip = 1)
    yield_brut_current <- read.csv(list_yield_current[simulation], skip = 1)
    
    yield_base_filtered <- yield_brut_base %>% filter(Time > cut_off_year_begin & Time < cut_off_year_end)
    yield_current_filtered <- yield_brut_current %>% filter(Time > cut_off_year_begin & Time < cut_off_year_end)
    
    lfi_base <- LFI(yield_base_filtered)
    lfi_current <- LFI(yield_current_filtered)
    
    lfi_ratio <- lfi_current / lfi_base
    return(lfi_ratio)
  })
  
  lfi_relative <- as.numeric(lfi_relative)
  return(lfi_relative)
}

# 初始化结果表格
# Initialize result table
LFI_all <- data.frame()

# 遍历每个监管情境
# Loop over regulation scenarios
for (regulation in regulation_scenarios) {
  
  # 构建路径
  # Build file paths for each deployment scenario
  results_path_1 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[1], "_", regulation), "Base", "output", "CIEM", "SizeIndicators")
  results_path_2 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[2], "_", regulation), "Base", "output", "CIEM", "SizeIndicators")
  results_path_3 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[3], "_", regulation), "Base", "output", "CIEM", "SizeIndicators")
  results_path_4 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[4], "_", regulation), "Base", "output", "CIEM", "SizeIndicators")
  
  results_path_scenario <- list(results_path_1, results_path_2, results_path_3, results_path_4)
  
  # 基线路径 / Baseline path
  results_path_base <- file.path("outputs/results_1111", "Base_simu", "Base", "output", "CIEM", "SizeIndicators")
  
  # 计算 LFI
  LFI_after_list <- map(results_path_scenario, ~ process_LFI(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[5],
    cut_off_year_end = n_years_cut[6]
  ))
  
  names(LFI_after_list) <- c("cost", "protection", "distance", "balance")
  
  # 转为数据框并标记当前监管情境
  # Convert to dataframe and tag with regulation scenario
  LFI_after_table <- stack(LFI_after_list) %>%
    mutate(regulation = regulation)
  
  # 合并结果
  # Append to overall table
  LFI_all <- rbind(LFI_all, LFI_after_table)
}

# 整理列名
# Rename columns
colnames(LFI_all) <- c("LFI_ratio", "deployment", "regulation")

# 翻译监管名称
# Translate regulation scenario labels
LFI_all$regulation <- factor(
  LFI_all$regulation,
  levels = c("sans_fermeture", "fermeture_chalut", "fermeture_totale"),
  labels = c("no closure", "trawlers closure", "complete closure")
)

# 方差齐性检验
# Levene's test for homogeneity of variance
leveneTest(LFI_ratio ~ regulation * deployment, data = LFI_all)

# 方差分析（ANOVA）
anova_model <- aov(LFI_ratio ~ regulation * deployment, data = LFI_all)
summary(anova_model)

# 正态性检验
# Shapiro-Wilk test for normality of residuals
shapiro.test(residuals(anova_model))

# 可视化诊断
# Diagnostic plots
qqnorm(residuals(anova_model))
qqline(residuals(anova_model), col = "red")

plot(fitted(anova_model), residuals(anova_model))
abline(h = 0, col = "blue")

# 非参数检验（Kruskal-Wallis）
# Kruskal-Wallis tests
kruskal.test(LFI_ratio ~ regulation, data = LFI_all)
kruskal.test(LFI_ratio ~ deployment, data = LFI_all)
