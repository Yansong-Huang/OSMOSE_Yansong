# t-test of biomass between OWF scenarios and baseline
# 比较风电场建设情景与基线模拟之间的生物量的 t 检验
# Author / 作者 : Yansong Huang
# Date created / 创建日期 : 2024-11-13

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

# Define a function to compute t-test for total biomass in a given period
# 定义函数，对指定时间段内总生物量进行 t 检验
t_test_biomass <- function(current_results_path, cut_off_year_begin, cut_off_year_end) {
  # List files / 获取文件列表
  list_biomass_base <- list.files(results_path_base, "Yansong_biomass_Simu.*csv", full.names = TRUE)
  list_biomass_current <- list.files(current_results_path, "Yansong_biomass_Simu.*csv", full.names = TRUE)
  
  # Process baseline / 处理基线模拟数据
  biomass_base <- lapply(1:n_replicate, function(simulation) {
    biomass_brut_base <- read.csv(list_biomass_base[simulation], skip = 1)
    biomass_total_base <- biomass_brut_base %>% 
      filter(Time > cut_off_year_begin) %>%
      filter(Time < cut_off_year_end) %>%
      colMeans() %>% sum()
    return(biomass_total_base)
  })
  
  # Process current OWF scenario / 处理当前情景数据
  biomass_current <- lapply(1:n_replicate, function(simulation) {
    biomass_brut_current <- read.csv(list_biomass_current[simulation], skip = 1)
    biomass_total_current <- biomass_brut_current %>% 
      filter(Time > cut_off_year_begin) %>%
      filter(Time < cut_off_year_end) %>%
      colMeans() %>% sum()
    return(biomass_total_current)
  })
  
  # Convert to numeric / 转换为数值向量
  biomass_base <- as.numeric(biomass_base)
  biomass_current <- as.numeric(biomass_current)
  
  # Perform t-test / 执行 t 检验
  t_test_result <- t.test(biomass_base, biomass_current)
  p_value <- t_test_result$p.value
  return(p_value)
}

# Initialize lists to store results / 初始化结果存储列表
biomass_during_list <- list()
biomass_after_list <- list()

# Loop over each regulation scenario / 遍历每种管控情景
for (regulation in regulation_scenarios){
  
  # Construct file paths for each deployment scenario / 构建每个情景的路径
  results_path_1 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[1], "_", regulation), "Base", "output", "CIEM")
  results_path_2 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[2], "_", regulation), "Base", "output", "CIEM")
  results_path_3 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[3], "_", regulation), "Base", "output", "CIEM")
  results_path_4 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[4], "_", regulation), "Base", "output", "CIEM")
  
  # Define baseline path / 定义基线路径
  results_path_base <<- file.path("outputs/results_1111", "Base_simu", "Base", "output", "CIEM")
  
  # Combine paths into a list / 合并为列表
  results_path_scenario <- list(results_path_1, results_path_2, results_path_3, results_path_4)
  
  # Apply function to calculate p-values for biomass during OWF construction
  # 计算建设期间的生物量 t 检验 p 值
  biomass_during_list <- map(results_path_scenario, ~ t_test_biomass(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[3],
    cut_off_year_end = n_years_cut[4]
  ))
  
  # Apply function to calculate p-values for biomass after OWF construction
  # 计算建设之后的生物量 t 检验 p 值
  biomass_after_list <- map(results_path_scenario, ~ t_test_biomass(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[5],
    cut_off_year_end = n_years_cut[6]
  ))
  
  # Name the results / 命名结果
  names(biomass_during_list) <- c("cost", "protection", "distance", "balance")
  names(biomass_after_list) <- c("cost", "protection", "distance", "balance")
  
  # Convert results to data frame and print / 转为数据框并打印
  print("during OWF construction")  
  biomass_during_table <- stack(biomass_during_list)
  colnames(biomass_during_table) <- c("p_value", "scenario")
  print(regulation)
  print(biomass_during_table)
  
  print("after OWF construction")   
  biomass_after_table <- stack(biomass_after_list)
  colnames(biomass_after_table) <- c("p_value", "scenario")
  print(regulation)
  print(biomass_after_table)
}
