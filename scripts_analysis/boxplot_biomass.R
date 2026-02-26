# ----------------------------------------------
# Boxplot of total biomass
# 总生物量箱线图
# Author: Yansong Huang
# Created on: 2024-10-31
# Description:
# This script reads total biomass csv outputs from simulations under different deployment and regulation scenarios,
# calculates total biomass ratios for specified periods, and visualizes the results using boxplots faceted by period and regulation.
# 本脚本读取不同部署和管控情景的总生物量csv输出数据，
# 计算指定时间段的总生物量比率，并用箱线图进行可视化，分面显示不同时间段和管理情景。
# ----------------------------------------------
rm(list=ls())

library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(purrr)
library(ncdf4)
library(patchwork)
library(egg)

# Global variables / 全局变量
deployment_scenarios <- c("cout","protection","loin","equilibre")   # Deployment scenarios / 部署情景
regulation_scenarios <- c("sans_fermeture","fermeture_chalut","fermeture_totale")  # Regulation scenarios / 管控情景
CC_scenarios <- c("ON","OFF")  # Climate Change scenarios (not used here) / 气候变化情景
n_years_cut <- c(10,21,22,34,35,49)  # Cutoff years for periods / 时间段分割点
n_replicate <- 30  # Number of simulation replicates / 模拟重复次数
total_biomass_base <- c(1322196,1334356,1334056)  # Base total biomass for normalization / 基线生物量

# Function to process biomass csv files for a scenario and time period
# 处理单个情景和时间段的生物量csv数据
process_biomass <- function(current_results_path, cut_off_year_begin, cut_off_year_end) {
  # List csv files matching pattern
  # 列出当前路径下所有符合模式的csv文件
  list_biomass_current <- list.files(current_results_path, "Yansong_biomass_Simu.*csv", full.names = TRUE)
  
  biomass_current_list <- lapply(1:n_replicate, function(simulation) {
    # Read csv skipping first line (header info)
    # 读取csv文件，跳过第一行
    biomass_brut_current <- read.csv(list_biomass_current[simulation], skip = 1)
    
    # Filter by time period and sum mean biomass across columns
    # 根据时间过滤并计算所有列的均值总和作为总生物量
    biomass_total_current <- biomass_brut_current %>% 
      filter(Time > cut_off_year_begin) %>%
      filter(Time < cut_off_year_end) %>%
      colMeans() %>%
      sum()
    
    return(biomass_total_current)
  })
  
  biomass_current <- as.numeric(unlist(biomass_current_list))
  return(biomass_current)
}

# Initialize global dataframe to store all results
# 初始化全局数据框，用于存储所有结果
total_biomass_all <- data.frame()

# Loop over each regulation scenario
# 遍历每个捕鱼管理情景
for (regulation in regulation_scenarios) {
  
  # Construct scenario paths for four deployment scenarios
  # 构建四个部署情景的结果路径
  results_path_1 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[1], "_", regulation), "Base", "output", "CIEM")
  results_path_2 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[2], "_", regulation), "Base", "output", "CIEM")
  results_path_3 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[3], "_", regulation), "Base", "output", "CIEM")
  results_path_4 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[4], "_", regulation), "Base", "output", "CIEM")
  
  results_path_scenario <- list(results_path_1, results_path_2, results_path_3, results_path_4)
  
  # Calculate biomass during 2023-2034 period (indices from n_years_cut)
  # 计算2023-2034年期间的总生物量
  total_biomass_during_list <- map(results_path_scenario, ~ process_biomass(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[3],
    cut_off_year_end = n_years_cut[4]
  ))
  
  # Calculate biomass during 2035-2050 period
  # 计算2035-2050年期间的总生物量
  total_biomass_after_list <- map(results_path_scenario, ~ process_biomass(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[5],
    cut_off_year_end = n_years_cut[6]
  ))
  
  # Name each list by deployment scenario
  # 给列表元素命名，方便后续处理
  names(total_biomass_during_list) <- c("cost", "protection", "distance", "balance")
  names(total_biomass_after_list) <- c("cost", "protection", "distance", "balance")
  
  # Convert to data frames and normalize by base biomass
  # 转换为数据框，并相对于基线生物量进行归一化
  total_biomass_during_table <- stack(total_biomass_during_list) %>%
    mutate(values = values / total_biomass_base[2], period = "2023-2034", regulation = regulation)
  total_biomass_after_table <- stack(total_biomass_after_list) %>%
    mutate(values = values / total_biomass_base[3], period = "2035-2050", regulation = regulation)
  
  # Combine into global dataframe
  # 合并结果至全局数据框
  total_biomass_all <- rbind(
    total_biomass_all,
    total_biomass_during_table,
    total_biomass_after_table
  )
}

# Rename columns for clarity
# 重命名列名
colnames(total_biomass_all) <- c("biomass_ratio", "deployment", "period", "regulation")

# Convert regulation factor with descriptive labels
# 设置管理情景因子的水平和标签
total_biomass_all$regulation <- factor(
  total_biomass_all$regulation,
  levels = c("sans_fermeture", "fermeture_chalut", "fermeture_totale"),
  labels = c("no closure during operational phase", "trawlers closure during operational phase", "complete closure during operational phase")
)

# Plot combined boxplot of biomass ratios
# 绘制总生物量相对比率的组合箱线图
combined_boxplot <- ggplot(total_biomass_all, aes(x = deployment, y = biomass_ratio - 1, fill = deployment)) +
  stat_summary(
    fun.data = "median_hilow", geom = "errorbar",
    aes(ymin = ..ymin.., ymax = ..ymax..), width = 0.2, color = "black"
  ) + # Whiskers / 须线
  geom_boxplot(varwidth = TRUE, outlier.shape = NA, linetype = "blank") + # Boxplot without outliers / 箱线图，无异常值点
  stat_summary(
    fun = mean, geom = "errorbar",
    aes(ymin = ..y.., ymax = ..y..), width = 0.75, color = "black"
  ) + # Mean line / 平均线
  geom_hline(yintercept = 0, color = "black", linetype = "dotted") + # Reference line at 0 / 参考线
  facet_grid(period ~ regulation, scales = "free_y", labeller = labeller(
    period = label_wrap_gen(20), regulation = label_wrap_gen(25)
  )) + # Facet by period and regulation / 分面：时间段和管控情景
  scale_fill_manual(
    values = c("purple", "pink", "orange", "lightblue"),
    labels = c("Cost minimisation", "Exclusion from environmental protection zones", "Long distance from the coast", "Balance")
  ) + # Custom fill colors and labels / 自定义填充色和标签
  labs(
    x = "Deployment Scenario",
    y = "Total fish biomass relative to baseline",
    fill = "OWF deployment scenario"
  ) + # Axis and legend labels / 轴标签和图例
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11),
    legend.position = "right",
    legend.direction = "vertical"
  ) +
  # 为特定分面单独定义星号数据
  geom_text(
    data = subset(total_biomass_all, period == "2023-2034" & regulation == "no closure during operational phase"),
    aes(x = 1, y = 0.03, label = "***"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_biomass_all, period == "2035-2050" & regulation == "no closure during operational phase"),
    aes(x = 1, y = 0.02, label = "***"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_biomass_all, period == "2035-2050" & regulation == "no closure during operational phase"),
    aes(x = 2, y = 0.02, label = "***"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_biomass_all, period == "2023-2034" & regulation == "no closure during operational phase"),
    aes(x = 3, y = 0.03, label = "*"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_biomass_all, period == "2035-2050" & regulation == "no closure during operational phase"),
    aes(x = 3, y = 0.02, label = "***"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_biomass_all, period == "2023-2034" & regulation == "no closure during operational phase"),
    aes(x = 4, y = 0.03, label = "*"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_biomass_all, period == "2035-2050" & regulation == "no closure during operational phase"),
    aes(x = 4, y = 0.02, label = "***"),
    inherit.aes = FALSE, size = 4
  )+
  geom_text(
    data = subset(total_biomass_all, period == "2023-2034" & regulation == "trawlers closure during operational phase"),
    aes(x = 1, y = 0.03, label = "*"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_biomass_all, period == "2023-2034" & regulation == "trawlers closure during operational phase"),
    aes(x = 4, y = 0.03, label = "**"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_biomass_all, period == "2035-2050" & regulation == "trawlers closure during operational phase"),
    aes(x = 1, y = 0.02, label = "***"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_biomass_all, period == "2035-2050" & regulation == "trawlers closure during operational phase"),
    aes(x = 2, y = 0.02, label = "***"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_biomass_all, period == "2035-2050" & regulation == "trawlers closure during operational phase"),
    aes(x = 3, y = 0.02, label = "***"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_biomass_all, period == "2035-2050" & regulation == "trawlers closure during operational phase"),
    aes(x = 4, y = 0.02, label = "***"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_biomass_all, period == "2023-2034" & regulation == "complete closure during operational phase"),
    aes(x = 1, y = 0.03, label = "*"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_biomass_all, period == "2023-2034" & regulation == "complete closure during operational phase"),
    aes(x = 2, y = 0.03, label = "***"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_biomass_all, period == "2023-2034" & regulation == "complete closure during operational phase"),
    aes(x = 3, y = 0.03, label = "**"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_biomass_all, period == "2023-2034" & regulation == "complete closure during operational phase"),
    aes(x = 4, y = 0.03, label = "*"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_biomass_all, period == "2035-2050" & regulation == "complete closure during operational phase"),
    aes(x = 1, y = 0.02, label = "***"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_biomass_all, period == "2035-2050" & regulation == "complete closure during operational phase"),
    aes(x = 2, y = 0.02, label = "***"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_biomass_all, period == "2035-2050" & regulation == "complete closure during operational phase"),
    aes(x = 3, y = 0.02, label = "***"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_biomass_all, period == "2035-2050" & regulation == "complete closure during operational phase"),
    aes(x = 4, y = 0.02, label = "***"),
    inherit.aes = FALSE, size = 4
  )
  
  # Tag facets with letters for easier referencing
  # 给分面标签加字母，方便引用
  tagged_facet <- tag_facet(combined_boxplot, 
                            open = "(", close = ")", tag_pool = letters, 
                            x = Inf, y = -Inf, 
                            hjust = 1.5, vjust = -1, 
                            fontface = "plain")

final_plot <- tagged_facet + theme(strip.text = element_text())

# Save final plot to file
# 保存最终图形
ggsave(
  file.path("figures", "publication", "boxplot", "final_version", "total_biomass_slide.png"),
  final_plot,
  width = 12, height = 6,  # 双栏宽度（英寸）
  dpi = 300
)
