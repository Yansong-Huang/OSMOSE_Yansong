# t-test of LFI catch ≥ 40cm
# 比较大体型鱼类（≥ 40cm）LFI 指标在 OWF 情景和参考模拟间的差异
# Author / 作者 : Yansong Huang
# Date created / 创建日期 : 2024-11-15

library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(purrr)
library(ncdf4)
library(patchwork)

# Global variables / 全局变量
deployment_scenarios <- c("cout","protection","loin","equilibre")
regulation_scenarios <- c("sans_fermeture","fermeture_chalut","fermeture_totale")
n_years_cut <- c(10,21,22,34,35,49)
n_replicate <- 30

# Define function to calculate LFI from yield size data
# 定义函数，计算大体型指数（LFI）
LFI <- function(data_yield_size, thresholds = 40) {
  data_yield_size_40 <- data_yield_size %>%
    filter(Size >= thresholds)
  
  # Total yield per year across all species and sizes / 所有物种所有体型年度总产量
  total_yield <- data_yield_size %>%
    group_by(Time) %>%
    summarize(total_yield = sum(across(2:17)))
  
  # Yield for large individuals / 大型个体产量
  large_yield <- data_yield_size_40 %>%
    group_by(Time) %>%
    summarize(large_yield = sum(across(2:17)))
  
  # LFI time series / LFI 时间序列
  lfi_series = large_yield$large_yield / total_yield$total_yield
  # Mean LFI over all years / 所有年份的 LFI 平均值
  lfi_mean = mean(lfi_series, na.rm = TRUE)
  
  return(lfi_mean)
}

# Function to compute t-test on LFI between current and baseline scenarios
# 比较当前情景与基线模拟间的 LFI 差异，进行 t 检验
t_test_lfi <- function(current_results_path, cut_off_year_begin, cut_off_year_end) {
  # Get file lists / 获取文件列表
  list_yield_base <- list.files(results_path_base, "Yansong_yieldDistribBySize_Simu.*csv", full.names = TRUE)
  list_yield_current <- list.files(current_results_path, "Yansong_yieldDistribBySize_Simu.*csv", full.names = TRUE)
  
  # Compute LFI for baseline / 基线情景的 LFI
  lfi_base <- lapply(1:n_replicate, function(simulation) {
    yield_brut_base <- read.csv(list_yield_base[simulation], skip = 1)
    yield_base_filtered <- yield_brut_base %>% filter(Time > cut_off_year_begin & Time < cut_off_year_end)
    lfi_base <- LFI(yield_base_filtered)
    return(lfi_base)
  })
  
  # Compute LFI for current scenario / 当前情景的 LFI
  lfi_current <- lapply(1:n_replicate, function(simulation) {
    yield_brut_current <- read.csv(list_yield_current[simulation], skip = 1)
    yield_current_filtered <- yield_brut_current %>% filter(Time > cut_off_year_begin & Time < cut_off_year_end)
    lfi_current <- LFI(yield_current_filtered)
    return(lfi_current)
  })
  
  # Convert to numeric / 转为数值向量
  lfi_base <- as.numeric(lfi_base)
  lfi_current <- as.numeric(lfi_current)
  
  # Perform t-test / 执行 t 检验
  t_test_result <- t.test(lfi_base, lfi_current)
  p_value <- t_test_result$p.value
  
  return(p_value)
}

# Initialize list to store t-test results / 初始化结果列表
lfi_period_list <- list()

# Loop through regulation scenarios / 遍历管控情景
for (regulation in regulation_scenarios){
  
  # Build file paths for each deployment scenario / 构建不同布设情景的路径
  results_path_1 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[1], "_", regulation), "Base", "output", "CIEM","SizeIndicators")
  results_path_2 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[2], "_", regulation), "Base", "output", "CIEM","SizeIndicators")
  results_path_3 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[3], "_", regulation), "Base", "output", "CIEM","SizeIndicators")
  results_path_4 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[4], "_", regulation), "Base", "output", "CIEM","SizeIndicators")
  
  # Baseline path / 基线路径
  results_path_base <- file.path("outputs/results_1111", "Base_simu", "Base", "output", "CIEM","SizeIndicators")
  # Combine all paths / 合并所有路径
  results_path_scenario <- list(results_path_1, results_path_2, results_path_3, results_path_4)
  
  # Apply function to each scenario / 对每个布设情景执行函数
  lfi_period_list <- map(results_path_scenario, ~ t_test_lfi(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[3],
    cut_off_year_end = n_years_cut[4]
  ))
  
  # Name results / 命名结果
  names(lfi_period_list) <- c("cost", "protection", "distance", "balance")
  
  # Convert to dataframe and print / 转为数据框并打印
  lfi_period_table <- stack(lfi_period_list)
  colnames(lfi_period_table) <- c("p_value", "scenario")
  print(regulation)
  print(lfi_period_table)
}
