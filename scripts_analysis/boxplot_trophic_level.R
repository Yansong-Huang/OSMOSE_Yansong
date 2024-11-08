# Boxplot of average trophic level
# Auteur : Yansong Huang
# Date de création : 2024-10-31

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
CC_scenarios <- c("ON","OFF")
n_years_cut <- c(10,21,22,34,35,49)
n_replicate <- 30

# calculate weighted trophic level
weighted_avg_tl <- function(tl_data, biomass_data) {
  # 去掉时间列，对每一行进行加权平均
  total_biomass <- rowSums(biomass_data[, -1])
  weighted_tl <- rowSums(tl_data[, -1] * biomass_data[, -1]) / total_biomass
  # 对所有年份的数据求平均
  return(mean(weighted_tl, na.rm = TRUE))
}

process_tl <- function(base_results_path, current_results_path, cut_off_year_begin, cut_off_year_end) {
  # 获取文件列表
  base_trophic_path <- file.path(base_results_path,"Trophic")
  current_trophic_path <- file.path(current_results_path,"Trophic")
 
   list_tl_base <- list.files(base_trophic_path, "Yansong_meanTL_Simu.", full.names = TRUE)
  list_tl_current <- list.files(current_trophic_path, "Yansong_meanTL_Simu.", full.names = TRUE)
  list_biomass_base <- list.files(base_results_path, "Yansong_biomass_Simu.", full.names = TRUE)
  list_biomass_current <- list.files(current_results_path, "Yansong_biomass_Simu.", full.names = TRUE)
  
  # 计算相对营养级
  tl_relative <- lapply(1:n_replicate, function(simulation) {
    # 读取营养级和生物量数据
    tl_brut_base <- read.csv(list_tl_base[simulation], skip = 1)
    tl_brut_current <- read.csv(list_tl_current[simulation], skip = 1)
    biomass_brut_base <- read.csv(list_biomass_base[simulation], skip = 1)
    biomass_brut_current <- read.csv(list_biomass_current[simulation], skip = 1)
    
    # 筛选时间段
    tl_base_filtered <- tl_brut_base %>% filter(Time > cut_off_year_begin & Time < cut_off_year_end)
    tl_current_filtered <- tl_brut_current %>% filter(Time > cut_off_year_begin & Time < cut_off_year_end)
    biomass_base_filtered <- biomass_brut_base %>% filter(Time > cut_off_year_begin & Time < cut_off_year_end)
    biomass_current_filtered <- biomass_brut_current %>% filter(Time > cut_off_year_begin & Time < cut_off_year_end)
    
    # 计算加权平均营养级
    tl_average_base <- weighted_avg_tl(tl_base_filtered, biomass_base_filtered)
    tl_average_current <- weighted_avg_tl(tl_current_filtered, biomass_current_filtered)
    
    # 计算营养级比例
    tl_ratio <- tl_average_current / tl_average_base
    return(tl_ratio)
  })
  
  # 转换为数值向量
  tl_relative <- as.numeric(tl_relative)
  return(tl_relative)
}



# 初始化空列表，用于存储所有结果
average_tl_before_list <- list()
average_tl_during_list <- list()
average_tl_after_list <- list()

for (regulation in regulation_scenarios){
  
  # 构建每个场景的路径
  results_path_1 <- file.path("outputs/results_2510", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[1], "_", regulation), "Base", "output", "CIEM")
  results_path_2 <- file.path("outputs/results_2510", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[2], "_", regulation), "Base", "output", "CIEM")
  results_path_3 <- file.path("outputs/results_2510", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[3], "_", regulation), "Base", "output", "CIEM")
  results_path_4 <- file.path("outputs/results_2510", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[4], "_", regulation), "Base", "output", "CIEM")
  
  # 基础路径
  results_path_base <- file.path("outputs/results_2510", "Base_simu", "output", "CIEM")
  # 将路径合并为列表
  results_path_scenario <- list(results_path_1, results_path_2, results_path_3, results_path_4)
  
  
  # apply the function to four deployment scenarios, respectively for three periods (before, during and after OWF construction)
  library(purrr)
  
  average_tl_before_list <- map(results_path_scenario, ~ process_tl(
    base_results_path = results_path_base,
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[1],
    cut_off_year_end = n_years_cut[2]
  ))
  
  average_tl_during_list <- map(results_path_scenario, ~ process_tl(
    base_results_path = results_path_base,
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[3],
    cut_off_year_end = n_years_cut[4]
  ))
  
  average_tl_after_list <- map(results_path_scenario, ~ process_tl(
    base_results_path = results_path_base,
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[5],
    cut_off_year_end = n_years_cut[6]
  ))
  
  
  # 为结果命名
  names(average_tl_before_list) <- c("cost", "protection", "distance", "balance")
  names(average_tl_during_list) <- c("cost", "protection", "distance", "balance")
  names(average_tl_after_list) <- c("cost", "protection", "distance", "balance")
  
  # 将所有结果转换为数据框
  average_tl_before_table <- stack(average_tl_before_list)
  average_tl_during_table <- stack(average_tl_during_list)
  average_tl_after_table <- stack(average_tl_after_list)
  colnames(average_tl_before_table) <- c("relative_tl", "scenario")
  colnames(average_tl_during_table) <- c("relative_tl", "scenario")
  colnames(average_tl_after_table) <- c("relative_tl", "scenario")
  
  # create plot 
  tl_boxplot_before <- ggplot(average_tl_before_table)+
    geom_boxplot(aes(x = scenario, y = relative_tl, fill = scenario)) +
    geom_hline(yintercept = 1, color = "black", linetype = "dotted") + 
    ggtitle("before OWF construction")+
    ylab("average trophic level change")+
    ylim(0.98,1.02)+
    scale_fill_manual(
      values = c("purple", "pink", "orange", "lightblue"),
      labels = c("energy cost minimisation", "exclusion from environmental protection zones", "long distance from the coast ", "balance")) + 
    labs(fill = "deployment scenario")+
    theme_bw()+
    theme(  plot.title = element_text(size = 13),
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
    scale_fill_manual(
      values = c("purple", "pink", "orange", "lightblue"),
      labels = c("energy cost minimisation", "exclusion from environmental protection zones", "long distance from the coast ", "balance")) + 
    labs(fill = "deployment scenario")+
    theme_bw()+
    theme(  plot.title = element_text(size = 13),
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
    scale_fill_manual(
      values = c("purple", "pink", "orange", "lightblue"),
      labels = c("energy cost minimisation", "exclusion from environmental protection zones", "long distance from the coast ", "balance")) + 
    labs(fill = "deployment scenario")+
    theme_bw()+
    theme(  plot.title = element_text(size = 13),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = 13, angle = 45, hjust = 1),
            axis.text.y = element_text(size = 13),
            legend.title = element_text(size = 13),
            legend.text = element_text(size = 13))
  
  combined_boxplot <- tl_boxplot_before + tl_boxplot_during + tl_boxplot_after +
    plot_layout(guides = "collect") + 
    theme(legend.position = "right")
  print(combined_boxplot)
  
  
  ggsave(file.path("figures","publication","boxplot", regulation,
                   "average_trophic_level.png"), combined_boxplot, width = 15, height = 4, dpi = 600)
  
}


