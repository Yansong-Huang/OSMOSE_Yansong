# t-test of biomass between OWF scenarios and reference simulations
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


t_test_biomass <- function(current_results_path, cut_off_year_begin, cut_off_year_end) {
  # 获取文件列表
  list_biomass_base <- list.files(results_path_base, "Yansong_biomass_Simu.*csv", full.names = TRUE)
  list_biomass_current <- list.files(current_results_path, "Yansong_biomass_Simu.*csv", full.names = TRUE)
  
  biomass_base <- lapply(1:n_replicate, function(simulation) {
    # 读取生物量数据
    biomass_brut_base <- read.csv(list_biomass_base[simulation], skip = 1)
    # 筛选时间段并计算总生物量
    biomass_total_base <- biomass_brut_base %>% 
      filter(Time>cut_off_year_begin)%>%
      filter(Time<cut_off_year_end)%>% 
      colMeans() %>% sum()
    
    return(biomass_total_base)
  })
  
  biomass_current <- lapply(1:n_replicate, function(simulation) {
    # 读取生物量数据
    biomass_brut_current <- read.csv(list_biomass_current[simulation], skip = 1)
    
    # 筛选时间段并计算总生物量
    biomass_total_current <- biomass_brut_current %>% 
      filter(Time>cut_off_year_begin)%>%
      filter(Time<cut_off_year_end)%>% 
      colMeans() %>% sum()
    return(biomass_total_current)
  })
  
  # 转换为数值向量
  biomass_base <- as.numeric(biomass_base)
  biomass_current <- as.numeric(biomass_current)
  
  # 计算统计量
  t_test_result <- t.test(biomass_base, biomass_current, alternative = "greater")
  p_value <- t_test_result$p.value
  
  return(p_value)
}


# 初始化空列表，用于存储所有结果
biomass_during_list <- list()
biomass_after_list <- list()

for (regulation in regulation_scenarios[1:2]){
  
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
  
  biomass_during_list <- map(results_path_scenario, ~ t_test_biomass(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[3],
    cut_off_year_end = n_years_cut[4]
  ))
  
  biomass_after_list <- map(results_path_scenario, ~ t_test_biomass(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[5],
    cut_off_year_end = n_years_cut[6]
  ))
  
  
  # 为结果命名
  names(biomass_during_list) <- c("cost", "protection", "distance", "balance")
  names(biomass_after_list) <- c("cost", "protection", "distance", "balance")
  
  # 将所有结果转换为数据框
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