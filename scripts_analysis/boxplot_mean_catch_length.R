# Boxplot of mean catch length
# Auteur : Yansong Huang
# Date de création : 2024-11-08

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
weighted_avg_mcl <- function(mcl_data, yield_data) {
  # 将 mcl_data 和 biomass_data 中的 NA 替换为 0
  mcl_data_no_time <- mcl_data[, -1] %>% mutate_all(~replace_na(., 0))
  yield_data_no_time <- yield_data[, -1] %>% mutate_all(~replace_na(., 0))
  
  # 计算每一行的总生物量
  total_yield <- rowSums(yield_data_no_time)
  
  # 加权捕捞体长
  weighted_mcl <- rowSums(mcl_data_no_time * yield_data_no_time) / total_yield
  
  # 对所有年份的数据求平均
  return(mean(weighted_mcl, na.rm = TRUE))
}

process_mcl <- function(base_results_path, current_results_path, cut_off_year_begin, cut_off_year_end) {
  # 获取文件列表
  base_trophic_path <- file.path(base_results_path,"SizeIndicators")
  current_trophic_path <- file.path(current_results_path,"SizeIndicators")
  
  list_mcl_base <- list.files(base_trophic_path, "Yansong_meanSizeCatch_Simu.", full.names = TRUE)
  list_mcl_current <- list.files(current_trophic_path, "Yansong_meanSizeCatch_Simu.", full.names = TRUE)
  list_yield_base <- list.files(base_results_path, "Yansong_yield_Simu.", full.names = TRUE)
  list_yield_current <- list.files(current_results_path, "Yansong_yield_Simu.", full.names = TRUE)
  
  # 遍历所有重复
  mcl_relative <- lapply(1:n_replicate, function(simulation) {
    # 读取捕捞体长和生物量数据
    mcl_brut_base <- read.csv(list_mcl_base[simulation], skip = 1)
    mcl_brut_current <- read.csv(list_mcl_current[simulation], skip = 1)
    yield_brut_base <- read.csv(list_yield_base[simulation], skip = 1)
    yield_brut_current <- read.csv(list_yield_current[simulation], skip = 1)
    
    # 筛选时间段
    mcl_base_filtered <- mcl_brut_base %>% filter(Time > cut_off_year_begin & Time < cut_off_year_end)
    mcl_current_filtered <- mcl_brut_current %>% filter(Time > cut_off_year_begin & Time < cut_off_year_end)
    yield_base_filtered <- yield_brut_base %>% filter(Time > cut_off_year_begin & Time < cut_off_year_end)
    yield_current_filtered <- yield_brut_current %>% filter(Time > cut_off_year_begin & Time < cut_off_year_end)
    
    # 计算加权捕捞体长
    mcl_average_base <- weighted_avg_mcl(mcl_base_filtered, yield_base_filtered)
    mcl_average_current <- weighted_avg_mcl(mcl_current_filtered, yield_current_filtered)
    
    # 计算捕捞体长比例
    mcl_ratio <- mcl_average_current / mcl_average_base
    return(mcl_ratio)
  })
  
  # 转换为数值向量
  mcl_relative <- as.numeric(mcl_relative)
  return(mcl_relative)
}



# 初始化空列表，用于存储所有结果
average_mcl_before_list <- list()
average_mcl_during_list <- list()
average_mcl_after_list <- list()

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
  
  average_mcl_before_list <- map(results_path_scenario, ~ process_mcl(
    base_results_path = results_path_base,
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[1],
    cut_off_year_end = n_years_cut[2]
  ))
  
  average_mcl_during_list <- map(results_path_scenario, ~ process_mcl(
    base_results_path = results_path_base,
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[3],
    cut_off_year_end = n_years_cut[4]
  ))
  
  average_mcl_after_list <- map(results_path_scenario, ~ process_mcl(
    base_results_path = results_path_base,
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[5],
    cut_off_year_end = n_years_cut[6]
  ))
  
  
  # 为结果命名
  names(average_mcl_before_list) <- c("cost", "protection", "distance", "balance")
  names(average_mcl_during_list) <- c("cost", "protection", "distance", "balance")
  names(average_mcl_after_list) <- c("cost", "protection", "distance", "balance")
  
  # 将所有结果转换为数据框
  average_mcl_before_table <- stack(average_mcl_before_list)
  average_mcl_during_table <- stack(average_mcl_during_list)
  average_mcl_after_table <- stack(average_mcl_after_list)
  colnames(average_mcl_before_table) <- c("relative_mcl", "scenario")
  colnames(average_mcl_during_table) <- c("relative_mcl", "scenario")
  colnames(average_mcl_after_table) <- c("relative_mcl", "scenario")
  
  # create plot 
  mcl_boxplot_before <- ggplot(average_mcl_before_table)+
    geom_boxplot(aes(x = scenario, y = relative_mcl, fill = scenario)) +
    geom_hline(yintercept = 1, color = "black", linetype = "dotted") + 
    ggtitle(regulation)+
    ylab("mean catch length change")+
    ylim(0.9,1.12)+
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
  
  mcl_boxplot_during <- ggplot(average_mcl_during_table)+
    geom_boxplot(aes(x = scenario, y = relative_mcl, fill = scenario)) +
    geom_hline(yintercept = 1, color = "black", linetype = "dotted") + 
    # ggtitle("during OWF construction")+
    ylim(0.9,1.12)+
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
  
  mcl_boxplot_after <- ggplot(average_mcl_after_table)+
    geom_boxplot(aes(x = scenario, y = relative_mcl, fill = scenario)) +
    geom_hline(yintercept = 1, color = "black", linetype = "dotted") + 
    # ggtitle("after OWF construction")+
    ylim(0.9,1.12)+
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
  
  combined_boxplot <- mcl_boxplot_before + mcl_boxplot_during + mcl_boxplot_after +
    plot_layout(guides = "collect") + 
    theme(legend.position = "right")
  print(combined_boxplot)
  
  
  ggsave(file.path("figures","publication","boxplot", regulation,
                   "mean_catch_length.png"), combined_boxplot, width = 15, height = 4, dpi = 600)
  
}


