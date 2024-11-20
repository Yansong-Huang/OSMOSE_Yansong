# t-test of yield between OWF scenarios and reference simulations
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


t_test_yield <- function(current_results_path, cut_off_year_begin, cut_off_year_end) {
  # 获取文件列表
  list_yield_base <- list.files(results_path_base, "Yansong_yield_Simu.*csv", full.names = TRUE)
  list_yield_current <- list.files(current_results_path, "Yansong_yield_Simu.*csv", full.names = TRUE)
  
  yield_base <- lapply(1:n_replicate, function(simulation) {
    # 读取生物量数据
    yield_brut_base <- read.csv(list_yield_base[simulation], skip = 1)
    # 筛选时间段并计算总生物量
    yield_total_base <- yield_brut_base %>% 
      filter(Time>cut_off_year_begin)%>%
      filter(Time<cut_off_year_end)%>% 
      colMeans() %>% sum()
    
    return(yield_total_base)
  })
  
  yield_current <- lapply(1:n_replicate, function(simulation) {
    # 读取生物量数据
    yield_brut_current <- read.csv(list_yield_current[simulation], skip = 1)
    
    # 筛选时间段并计算总生物量
    yield_total_current <- yield_brut_current %>% 
      filter(Time>cut_off_year_begin)%>%
      filter(Time<cut_off_year_end)%>% 
      colMeans() %>% sum()
    return(yield_total_current)
  })
  
  # 转换为数值向量
  yield_base <- as.numeric(yield_base)
  yield_current <- as.numeric(yield_current)
  
  # 计算统计量
  t_test_result <- t.test(yield_base, yield_current)
  p_value <- t_test_result$p.value
  
  return(p_value)
}


# 初始化空列表，用于存储所有结果
yield_during_list <- list()
yield_after_list <- list()

for (regulation in regulation_scenarios){
  
  # 构建每个场景的路径
  results_path_1 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[1], "_", regulation), "Base", "output", "CIEM")
  results_path_2 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[2], "_", regulation), "Base", "output", "CIEM")
  results_path_3 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[3], "_", regulation), "Base", "output", "CIEM")
  results_path_4 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[4], "_", regulation), "Base", "output", "CIEM")
  
  # 基础路径
  results_path_base <- file.path("outputs/results_1111", "Base_simu", "Base", "output", "CIEM")
  # 将路径合并为列表
  results_path_scenario <- list(results_path_1, results_path_2, results_path_3, results_path_4)
  
  
  # apply the function to four deployment scenarios, respectively for period after OWF construction
  
  yield_during_list <- map(results_path_scenario, ~ t_test_yield(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[3],
    cut_off_year_end = n_years_cut[4]
  ))
  
  yield_after_list <- map(results_path_scenario, ~ t_test_yield(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[5],
    cut_off_year_end = n_years_cut[6]
  ))
  
  
  # 为结果命名
  names(yield_during_list) <- c("cost", "protection", "distance", "balance")
  names(yield_after_list) <- c("cost", "protection", "distance", "balance")
  
  # 将所有结果转换为数据框
  print("during OWF construction")
  yield_during_table <- stack(yield_during_list)
  colnames(yield_during_table) <- c("p_value", "scenario")
  print(regulation)
  print(yield_during_table)
  print("after OWF construction")
  yield_after_table <- stack(yield_after_list)
  colnames(yield_after_table) <- c("p_value", "scenario")
  print(regulation)
  print(yield_after_table)
}