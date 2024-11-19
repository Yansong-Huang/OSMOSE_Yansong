# t-test of biomass between OWF scenarios
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


t_test_biomass <- function(deployment, cut_off_year_begin, cut_off_year_end) {
  # 获取文件列表
  results_path_no_closure <- file.path("outputs/results_1111", paste0("CC.ON_", deployment, "_", regulation_scenarios[1]), "Base", "output", "CIEM")
  results_path_trawlers_closure <- file.path("outputs/results_1111", paste0("CC.ON_", deployment, "_", regulation_scenarios[2]), "Base", "output", "CIEM")
  
  list_biomass_no_closure <- list.files(results_path_no_closure, "Yansong_biomass_Simu.*csv", full.names = TRUE)
  list_biomass_trawlers_closure <- list.files(results_path_trawlers_closure, "Yansong_biomass_Simu.*csv", full.names = TRUE)
  
  biomass_no_closure <- lapply(1:n_replicate, function(simulation) {
    # 读取生物量数据
    biomass_brut_no_closure <- read.csv(list_biomass_no_closure[simulation], skip = 1)
    # 筛选时间段并计算总生物量
    biomass_total_no_closure <- biomass_brut_no_closure %>% 
      filter(Time>cut_off_year_begin)%>%
      filter(Time<cut_off_year_end)%>% 
      select(-Time) %>%
      colMeans() %>% sum()
    
    return(biomass_total_no_closure)
  })
  
  biomass_trawlers_closure <- lapply(1:n_replicate, function(simulation) {
    # 读取生物量数据
    biomass_brut_trawlers_closure <- read.csv(list_biomass_trawlers_closure[simulation], skip = 1)
    
    # 筛选时间段并计算总生物量
    biomass_total_trawlers_closure <- biomass_brut_trawlers_closure %>% 
      filter(Time>cut_off_year_begin)%>%
      filter(Time<cut_off_year_end)%>% 
      select(-Time) %>%
      colMeans() %>% sum()
    return(biomass_total_trawlers_closure)
  })
  
  # 转换为数值向量
  biomass_no_closure <- as.numeric(biomass_no_closure)
  biomass_trawlers_closure <- as.numeric(biomass_trawlers_closure)
  
  # 计算统计量
  t_test_result <- t.test(biomass_no_closure, biomass_trawlers_closure, alternative = "less")
  p_value <- t_test_result$p.value
  
  return(p_value)
}


# 初始化空列表，用于存储所有结果
biomass_after_list <- list()
  

  # apply the function to four deployment scenarios, respectively for period after OWF construction
  biomass_after_list <- map(deployment_scenarios, ~ t_test_biomass(
    deployment = .x,
    cut_off_year_begin = n_years_cut[5],
    cut_off_year_end = n_years_cut[6]
  ))
  
  
  # 为结果命名
  names(biomass_after_list) <- c("cost", "protection", "distance", "balance")
  
  # 将所有结果转换为数据框
  print("after OWF construction")
  biomass_after_table <- stack(biomass_after_list)
  colnames(biomass_after_table) <- c("p_value", "scenario")
  print(biomass_after_table)