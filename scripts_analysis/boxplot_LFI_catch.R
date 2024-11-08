# Boxplot of LFI 40 in catch
# Auteur : Yansong Huang
# Date de création : 2024-11-06

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

LFI <- function(data_yield_size, thresholds = 40) {
  data_yield_size_40 <- data_yield_size %>%
    filter(Size >= 40)
  # 求总生物量和大鱼生物量
  # 按时间汇总所有物种和体型的生物量总和
  total_yield <- data_yield_size %>%
    group_by(Time) %>%
    summarize(total_yield = sum(across(2:17)))
  
  # 按时间汇总所有物种和体型的生物量总和
  large_yield <- data_yield_size_40 %>%
    group_by(Time) %>%
    summarize(large_yield = sum(across(2:17)))
  
  # calculate a time serie of LFI
  lfi_series = large_yield$large_yield / total_yield$total_yield
  # calculate mean LFI of all years
  lfi_mean = mean(lfi_series, na.rm = TRUE)
  
  return(lfi_mean)
}


process_lfi <- function(current_results_path, cut_off_year_begin, cut_off_year_end) {
  # 获取文件列表
  list_yield_base <- list.files(results_path_base, "Yansong_yieldDistribBySize_Simu.", full.names = TRUE)
  list_yield_current <- list.files(current_results_path, "Yansong_yieldDistribBySize_Simu.", full.names = TRUE)
  
  # 计算相对
  lfi_relative <- lapply(1:n_replicate, function(simulation) {
    # 读取生物量数据
    yield_brut_base <- read.csv(list_yield_base[simulation], skip = 1)
    yield_brut_current <- read.csv(list_yield_current[simulation], skip = 1)
    
    # 筛选时间段
    yield_base_filtered <- yield_brut_base %>% filter(Time > cut_off_year_begin & Time < cut_off_year_end)
    yield_current_filtered <- yield_brut_current %>% filter(Time > cut_off_year_begin & Time < cut_off_year_end)
    
    # 计算LFI
    lfi_base <- LFI(yield_base_filtered)
    lfi_current <- LFI(yield_current_filtered)
    
    # 计算营养级比例
    lfi_ratio <- lfi_current / lfi_base
    return(lfi_ratio)
  })
  
  # 转换为数值向量
  lfi_relative <- as.numeric(lfi_relative)
  return(lfi_relative)
}



# 初始化空列表，用于存储所有结果
lfi_before_list <- list()
lfi_during_list <- list()
lfi_after_list <- list()

for (regulation in regulation_scenarios){
  
  # 构建每个场景的路径
  results_path_1 <- file.path("outputs/results_2510", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[1], "_", regulation), "Base", "output", "CIEM","SizeIndicators")
  results_path_2 <- file.path("outputs/results_2510", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[2], "_", regulation), "Base", "output", "CIEM","SizeIndicators")
  results_path_3 <- file.path("outputs/results_2510", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[3], "_", regulation), "Base", "output", "CIEM","SizeIndicators")
  results_path_4 <- file.path("outputs/results_2510", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[4], "_", regulation), "Base", "output", "CIEM","SizeIndicators")
  
  # 基础路径
  results_path_base <- file.path("outputs/results_2510", "Base_simu", "output", "CIEM","SizeIndicators")
  # 将路径合并为列表
  results_path_scenario <- list(results_path_1, results_path_2, results_path_3, results_path_4)
  
  
  # apply the function to four deployment scenarios, respectively for three periods (before, during and after OWF construction)
  library(purrr)
  
  lfi_before_list <- map(results_path_scenario, ~ process_lfi(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[1],
    cut_off_year_end = n_years_cut[2]
  ))
  
  lfi_during_list <- map(results_path_scenario, ~ process_lfi(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[3],
    cut_off_year_end = n_years_cut[4]
  ))
  
  lfi_after_list <- map(results_path_scenario, ~ process_lfi(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[5],
    cut_off_year_end = n_years_cut[6]
  ))
  
  
  # 为结果命名
  names(lfi_before_list) <- c("cost", "protection", "distance", "balance")
  names(lfi_during_list) <- c("cost", "protection", "distance", "balance")
  names(lfi_after_list) <- c("cost", "protection", "distance", "balance")
  
  # 将所有结果转换为数据框
  lfi_before_table <- stack(lfi_before_list)
  lfi_during_table <- stack(lfi_during_list)
  lfi_after_table <- stack(lfi_after_list)
  colnames(lfi_before_table) <- c("relative_lfi", "scenario")
  colnames(lfi_during_table) <- c("relative_lfi", "scenario")
  colnames(lfi_after_table) <- c("relative_lfi", "scenario")
  
  # create plot 
  lfi_boxplot_before <- ggplot(lfi_before_table)+
    geom_boxplot(aes(x = scenario, y = relative_lfi, fill = scenario)) +
    geom_hline(yintercept = 1, color = "black", linetype = "dotted") + 
    ggtitle(regulation)+
    ylab("LFI catch change")+
    # ylim(0.8,1.9)+
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
  
  lfi_boxplot_during <- ggplot(lfi_during_table)+
    geom_boxplot(aes(x = scenario, y = relative_lfi, fill = scenario)) +
    geom_hline(yintercept = 1, color = "black", linetype = "dotted") + 
    # ylim(0.8,1.9)+
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
  
  lfi_boxplot_after <- ggplot(lfi_after_table)+
    geom_boxplot(aes(x = scenario, y = relative_lfi, fill = scenario)) +
    geom_hline(yintercept = 1, color = "black", linetype = "dotted") + 
    # ylim(0.8,1.9)+
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
  
  combined_boxplot <- lfi_boxplot_before + lfi_boxplot_during + lfi_boxplot_after +
    plot_layout(guides = "collect") + 
    theme(legend.position = "right")
  print(combined_boxplot)
  
  
  # ggsave(file.path("figures","publication","boxplot", regulation,
  #                  "LFI_catch_40.png"), combined_boxplot, width = 15, height = 4, dpi = 600)
  
}

