# t-test of LFI 40
# Auteur : Yansong Huang
# Date de création : 2024-11-13

library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(purrr)
library(ncdf4)
library(patchwork)

# variable globales
deployment_scenarios <- c("cout","protection","loin","equilibre")
regulation_scenarios <- c("sans_fermeture","fermeture_chalut","fermeture_totale")
n_years_cut <- c(10,21,22,34,35,49)
n_replicate <- 30

LFI <- function(data_biomass_size, thresholds = 40) {
  data_biomass_size_40 <- data_biomass_size %>%
    filter(Size >= 40)
  # 求总生物量和大鱼生物量
  # 按时间汇总所有物种和体型的生物量总和
  total_biomass <- data_biomass_size %>%
    group_by(Time) %>%
    summarize(total_biomass = sum(across(2:17)))
  
  # 按时间汇总所有物种和体型的生物量总和
  large_biomass <- data_biomass_size_40 %>%
    group_by(Time) %>%
    summarize(large_biomass = sum(across(2:17)))
  
  # calculate a time serie of LFI
  lfi_series = large_biomass$large_biomass / total_biomass$total_biomass
  # calculate mean LFI of all years
  lfi_mean = mean(lfi_series, na.rm = TRUE)
  
  return(lfi_mean)
}


t_test_lfi <- function(current_results_path, cut_off_year_begin, cut_off_year_end) {
  # 获取文件列表
  list_biomass_base <- list.files(results_path_base, "Yansong_biomassDistribBySize_Simu.", full.names = TRUE)
  list_biomass_current <- list.files(current_results_path, "Yansong_biomassDistribBySize_Simu.", full.names = TRUE)
  
  # 计算相对营养级
  lfi_base <- lapply(1:n_replicate, function(simulation) {
    # 读取生物量数据
    biomass_brut_base <- read.csv(list_biomass_base[simulation], skip = 1)
    # 筛选时间段
    biomass_base_filtered <- biomass_brut_base %>% filter(Time > cut_off_year_begin & Time < cut_off_year_end)
    # 计算LFI
    lfi_base <- LFI(biomass_base_filtered)
    return(lfi_base)
  })
  
  lfi_current <- lapply(1:n_replicate, function(simulation) {
    # 读取生物量数据
    biomass_brut_current <- read.csv(list_biomass_current[simulation], skip = 1)
    
    # 筛选时间段
    biomass_current_filtered <- biomass_brut_current %>% filter(Time > cut_off_year_begin & Time < cut_off_year_end)
    
    # 计算LFI
    lfi_current <- LFI(biomass_current_filtered)
    return(lfi_current)
  })
  
  # 转换为数值向量
  lfi_base <- as.numeric(lfi_base)
  lfi_current <- as.numeric(lfi_current)
  
  # 计算统计量
  t_test_result <- t.test(lfi_base, lfi_current)
  p_value <- t_test_result$p.value
  
  return(p_value)
}


# 初始化空列表，用于存储所有结果
lfi_after_list <- list()

for (regulation in regulation_scenarios){
  
  # 构建每个场景的路径
  results_path_1 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[1], "_", regulation), "Base", "output", "CIEM","Indicators")
  results_path_2 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[2], "_", regulation), "Base", "output", "CIEM","Indicators")
  results_path_3 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[3], "_", regulation), "Base", "output", "CIEM","Indicators")
  results_path_4 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[4], "_", regulation), "Base", "output", "CIEM","Indicators")
  
  # 基础路径
  results_path_base <- file.path("outputs/results_1111", "Base_simu", "Base", "output", "CIEM","Indicators")
  # 将路径合并为列表
  results_path_scenario <- list(results_path_1, results_path_2, results_path_3, results_path_4)
  
  
  # apply the function to four deployment scenarios, respectively for period after OWF construction
  
  lfi_after_list <- map(results_path_scenario, ~ t_test_lfi(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[5],
    cut_off_year_end = n_years_cut[6]
  ))
  
  
  # 为结果命名
  names(lfi_after_list) <- c("cost", "protection", "distance", "balance")
  
  # 将所有结果转换为数据框
  lfi_after_table <- stack(lfi_after_list)
  colnames(lfi_after_table) <- c("p_value", "scenario")
  print(regulation)
  print(lfi_after_table)
}