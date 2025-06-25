# Boxplot of average trophic level
# Author : Yansong Huang
# Date of creation : 2024-10-31

library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(purrr)
library(ncdf4)
library(patchwork)

# ===== 全局变量 / Global variables =====
deployment_scenarios <- c("cout","protection","loin","equilibre")  # 投资方案
regulation_scenarios <- c("sans_fermeture","fermeture_chalut","fermeture_totale")  # 管理措施
CC_scenarios <- c("ON","OFF")  # 气候情景
n_years_cut <- c(10,21,22,34,35,49)  # 不同时间段的年份边界
n_replicate <- 30  # 重复模拟次数

# ===== 计算加权平均营养级 / Weighted average TL calculation =====
weighted_avg_tl <- function(tl_data, biomass_data) {
  # 去掉时间列，对每一行进行加权平均 / Remove time column and compute weighted mean per row
  total_biomass <- rowSums(biomass_data[, -1])
  weighted_tl <- rowSums(tl_data[, -1] * biomass_data[, -1]) / total_biomass
  # 对所有年份的数据求平均 / Average over all time steps
  return(mean(weighted_tl, na.rm = TRUE))
}

# ===== 提取并计算营养级相对变化 / Process and compare average TL =====
process_tl <- function(current_results_path, cut_off_year_begin, cut_off_year_end) {
  # 路径 / Paths
  base_trophic_path <- file.path(results_path_base, "Trophic")
  current_trophic_path <- file.path(current_results_path, "Trophic")
  
  # 查找文件 / List TL and biomass files
  list_tl_base <- list.files(base_trophic_path, "Yansong_meanTL_Simu.*csv", full.names = TRUE)
  list_tl_current <- list.files(current_trophic_path, "Yansong_meanTL_Simu.*csv", full.names = TRUE)
  list_biomass_base <- list.files(results_path_base, "Yansong_biomass_Simu.*csv", full.names = TRUE)
  list_biomass_current <- list.files(current_results_path, "Yansong_biomass_Simu.*csv", full.names = TRUE)
  
  # 针对每次模拟，计算加权营养级的相对变化 / Compute TL change for each replicate
  tl_relative <- lapply(1:n_replicate, function(simulation) {
    # 读取数据 / Read TL and biomass data
    tl_brut_base <- read.csv(list_tl_base[simulation], skip = 1)
    tl_brut_current <- read.csv(list_tl_current[simulation], skip = 1)
    biomass_brut_base <- read.csv(list_biomass_base[simulation], skip = 1)
    biomass_brut_current <- read.csv(list_biomass_current[simulation], skip = 1)
    
    # 筛选时间段 / Filter by selected years
    tl_base_filtered <- tl_brut_base %>% filter(Time > cut_off_year_begin & Time < cut_off_year_end)
    tl_current_filtered <- tl_brut_current %>% filter(Time > cut_off_year_begin & Time < cut_off_year_end)
    biomass_base_filtered <- biomass_brut_base %>% filter(Time > cut_off_year_begin & Time < cut_off_year_end)
    biomass_current_filtered <- biomass_brut_current %>% filter(Time > cut_off_year_begin & Time < cut_off_year_end)
    
    # 加权平均营养级 / Weighted average TL
    tl_average_base <- weighted_avg_tl(tl_base_filtered, biomass_base_filtered)
    tl_average_current <- weighted_avg_tl(tl_current_filtered, biomass_current_filtered)
    
    # 相对营养级变化 / Relative TL ratio
    tl_ratio <- tl_average_current / tl_average_base
    return(tl_ratio)
  })
  
  return(as.numeric(tl_relative))
}

# ===== 主循环：遍历所有调控方案 / Main loop over regulation scenarios =====
for (regulation in regulation_scenarios){
  # 构建每个场景的路径 / Build paths for each deployment scenario
  results_path_1 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[1], "_", regulation), "Base", "output", "CIEM")
  results_path_2 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[2], "_", regulation), "Base", "output", "CIEM")
  results_path_3 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[3], "_", regulation), "Base", "output", "CIEM")
  results_path_4 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[4], "_", regulation), "Base", "output", "CIEM")
  
  # 基础情景路径 / Base scenario path
  results_path_base <- file.path("outputs/results_1111", "Base_simu", "Base", "output", "CIEM")
  results_path_scenario <- list(results_path_1, results_path_2, results_path_3, results_path_4)
  
  # 三个时间段分别计算相对营养级 / Process three time periods
  average_tl_before_list <- map(results_path_scenario, ~ process_tl(.x, n_years_cut[1], n_years_cut[2]))
  average_tl_during_list <- map(results_path_scenario, ~ process_tl(.x, n_years_cut[3], n_years_cut[4]))
  average_tl_after_list <- map(results_path_scenario, ~ process_tl(.x, n_years_cut[5], n_years_cut[6]))
  
  # 命名每种方案 / Name scenarios
  scenario_names <- c("cost", "protection", "distance", "balance")
  names(average_tl_before_list) <- scenario_names
  names(average_tl_during_list) <- scenario_names
  names(average_tl_after_list) <- scenario_names
  
  # 整合为数据框 / Convert to data frames
  average_tl_before_table <- stack(average_tl_before_list)
  average_tl_during_table <- stack(average_tl_during_list)
  average_tl_after_table <- stack(average_tl_after_list)
  colnames(average_tl_before_table) <- c("relative_tl", "scenario")
  colnames(average_tl_during_table) <- c("relative_tl", "scenario")
  colnames(average_tl_after_table) <- c("relative_tl", "scenario")
  
  # 绘制图形 / Plot results
  tl_boxplot_before <- ggplot(average_tl_before_table)+
    geom_boxplot(aes(x = scenario, y = relative_tl, fill = scenario)) +
    geom_hline(yintercept = 1, color = "black", linetype = "dotted") + 
    ggtitle("before OWF construction")+
    ylab("average trophic level change")+
    ylim(0.98,1.02)+
    scale_fill_manual(values = c("purple", "pink", "orange", "lightblue"),
                      labels = c("energy cost minimisation", "exclusion from environmental protection zones", "long distance from the coast ", "balance")) + 
    labs(fill = "deployment scenario")+
    theme_bw() +
    theme(plot.title = element_text(size = 13),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 13),
          axis.text.x = element_text(size = 13, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 13),
          legend.title = element_text(size = 13),
          legend.text = element_text(size = 13))
  
  tl_boxplot_during <- ggplot(average_tl_during_table)+
    geom_boxplot(aes(x = scenario, y = relative_tl, fill = scenario)) +
    geom_hline(yintercept = 1, color = "black", linetype = "dotted") + 
    ggtitle("during OWF construction")+
    ylim(0.98,1.02)+
    scale_fill_manual(values = c("purple", "pink", "orange", "lightblue"),
                      labels = c("energy cost minimisation", "exclusion from environmental protection zones", "long distance from the coast ", "balance")) + 
    labs(fill = "deployment scenario")+
    theme_bw() +
    theme(plot.title = element_text(size = 13),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 13, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 13),
          legend.title = element_text(size = 13),
          legend.text = element_text(size = 13))
  
  tl_boxplot_after <- ggplot(average_tl_after_table)+
    geom_boxplot(aes(x = scenario, y = relative_tl, fill = scenario)) +
    geom_hline(yintercept = 1, color = "black", linetype = "dotted") + 
    ggtitle("after OWF construction")+
    ylim(0.98,1.02)+
    scale_fill_manual(values = c("purple", "pink", "orange", "lightblue"),
                      labels = c("energy cost minimisation", "exclusion from environmental protection zones", "long distance from the coast ", "balance")) + 
    labs(fill = "deployment scenario")+
    theme_bw() +
    theme(plot.title = element_text(size = 13),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 13, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 13),
          legend.title = element_text(size = 13),
          legend.text = element_text(size = 13))
  
  # 合并图形 / Combine plots
  combined_boxplot <- tl_boxplot_before + tl_boxplot_during + tl_boxplot_after +
    plot_layout(guides = "collect") + 
    theme(legend.position = "right")
  
  print(combined_boxplot)
  
  # 保存图像 / Save output
  ggsave(file.path("figures", "publication", "boxplot", regulation,
                   "average_trophic_level.png"),
         combined_boxplot, width = 15, height = 4, dpi = 600)
}
