# ----------------------------------------------
# Boxplot of LFI catch 40
# LFI 捕获比例（40cm以上个体）
# Auteur : Yansong Huang
# Date de création : 2024-11-06
# Description:
# This script calculates and visualizes the Large Fish Indicator (LFI) 
# based on size-distributed catch data for different deployment and regulation scenarios.
# 本脚本基于不同部署和管控情景的体型分布捕获数据，
# 计算并可视化大鱼指标（40cm及以上）。
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
deployment_scenarios <- c("cout","protection","loin","equilibre")  # 部署情景
regulation_scenarios <- c("sans_fermeture","fermeture_chalut","fermeture_totale")  # 管控情景
n_years_cut <- c(10,21,22,34,35,49)  # 时间段切割点
n_replicate <- 30  # 模拟重复次数
LFI_catch_base <- c(0.1255017,0.1032469,0.1388709)  # 基准LFI值用于归一化

# Function to calculate Large Fish Indicator (LFI)
# 计算大鱼指标函数（阈值默认40cm）
LFI <- function(data_yield_size, thresholds = 40) {
  # Filter for fish size >= threshold
  # 筛选体长≥40cm的个体
  data_yield_size_40 <- data_yield_size %>%
    filter(Size >= thresholds)
  
  # Calculate total yield by summing all species and sizes per time
  # 按时间汇总所有物种和体型的总捕获量
  total_yield <- data_yield_size %>%
    group_by(Time) %>%
    summarize(total_yield = sum(across(2:17)))
  
  # Calculate yield for large fish (≥threshold)
  # 计算大鱼的捕获量
  large_yield <- data_yield_size_40 %>%
    group_by(Time) %>%
    summarize(large_yield = sum(across(2:17)))
  
  # Compute time series of LFI = large yield / total yield
  # 计算大鱼比例时间序列
  lfi_series <- large_yield$large_yield / total_yield$total_yield
  
  # Calculate mean LFI across all years (removing NA)
  # 计算所有年份的平均LFI
  lfi_mean <- mean(lfi_series, na.rm = TRUE)
  
  return(lfi_mean)
}

# Process LFI for a given scenario folder and time period
# 读取指定路径下所有模拟数据，计算指定时间段的LFI均值
process_LFI <- function(current_results_path, cut_off_year_begin, cut_off_year_end) {
  # List CSV files matching pattern
  # 获取当前路径下所有符合条件的文件
  list_yield_current <- list.files(current_results_path, "Yansong_yieldDistribBySize_Simu.*csv", full.names = TRUE)
  
  # Calculate LFI for each replicate simulation
  # 对每次模拟计算LFI
  lfi_relative <- lapply(1:n_replicate, function(simulation) {
    # Read CSV, skip first metadata line
    yield_brut_current <- read.csv(list_yield_current[simulation], skip = 1)
    
    # Filter by time range
    yield_current_filtered <- yield_brut_current %>% filter(Time > cut_off_year_begin & Time < cut_off_year_end)
    
    # Calculate LFI for filtered data
    lfi_current <- LFI(yield_current_filtered)
    
    return(lfi_current)
  })
  
  # Convert list to numeric vector
  lfi_relative <- as.numeric(lfi_relative)
  return(lfi_relative)
}

# Initialize dataframe to store all LFI results
# 初始化数据框存储所有结果
LFI_catch_all <- data.frame()

# Loop over regulation scenarios to process data and combine results
# 遍历所有管控情景，处理数据并合并结果
for (regulation in regulation_scenarios) {
  
  # Construct file paths for each deployment scenario
  # 构建每个部署情景对应的路径
  results_path_1 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[1], "_", regulation), "Base", "output", "CIEM","SizeIndicators")
  results_path_2 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[2], "_", regulation), "Base", "output", "CIEM","SizeIndicators")
  results_path_3 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[3], "_", regulation), "Base", "output", "CIEM","SizeIndicators")
  results_path_4 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[4], "_", regulation), "Base", "output", "CIEM","SizeIndicators")
  
  results_path_scenario <- list(results_path_1, results_path_2, results_path_3, results_path_4)
  
  # Calculate LFI for period 2023-2034
  LFI_catch_during_list <- purrr::map(results_path_scenario, ~ process_LFI(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[3],
    cut_off_year_end = n_years_cut[4]
  ))
  
  # Calculate LFI for period 2035-2050
  LFI_catch_period_list <- purrr::map(results_path_scenario, ~ process_LFI(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[5],
    cut_off_year_end = n_years_cut[6]
  ))
  
  # Name list elements for clarity
  names(LFI_catch_during_list) <- c("cost", "protection", "distance", "balance")
  names(LFI_catch_period_list) <- c("cost", "protection", "distance", "balance")
  
  # Convert lists to data frames and normalize by base values
  LFI_catch_during_table <- stack(LFI_catch_during_list) %>%
    mutate(values = values / LFI_catch_base[2], period = "2023-2034", regulation = regulation)
  LFI_catch_period_table <- stack(LFI_catch_period_list) %>%
    mutate(values = values / LFI_catch_base[3], period = "2035-2050", regulation = regulation)
  
  # Combine into the global dataframe
  LFI_catch_all <- rbind(
    LFI_catch_all,
    LFI_catch_during_table,
    LFI_catch_period_table
  )
}

# Rename columns for clarity
colnames(LFI_catch_all) <- c("LFI_ratio", "deployment", "period", "regulation")

# Convert regulation to factor with descriptive labels
LFI_catch_all$regulation <- factor(
  LFI_catch_all$regulation,
  levels = c("sans_fermeture", "fermeture_chalut", "fermeture_totale"),
  labels = c("no closure during operational phase", "trawlers closure during operational phase", "complete closure during operational phase")
)

# Plot combined boxplot of LFI ratios
combined_boxplot <- ggplot(LFI_catch_all, aes(x = deployment, y = LFI_ratio - 1, fill = deployment)) +
  stat_summary(
    fun.data = "median_hilow",
    geom = "errorbar",
    aes(ymin = ..ymin.., ymax = ..ymax..),
    width = 0.2,
    color = "black"
  ) + # whiskers
  geom_boxplot(varwidth = TRUE, outlier.shape = NA, linetype = "blank") + # boxplot without outliers
  stat_summary(
    fun = mean,
    geom = "errorbar",
    aes(ymin = ..y.., ymax = ..y..),
    width = 0.75,
    color = "black"
  ) + # mean line
  geom_hline(yintercept = 0, color = "black", linetype = "dotted") + # reference line
  facet_grid(period ~ regulation, scales = "free_y", labeller = labeller(
    period = label_wrap_gen(20), regulation = label_wrap_gen(25)
  )) + # facet by period and regulation
  scale_fill_manual(
    values = c("purple", "pink", "orange", "lightblue"),
    labels = c("Cost minimisation", "Exclusion from environmental protection zones", "Long distance from the coast", "Balance")
  ) +
  labs(
    x = "Deployment scenario",
    y = "LFI catch relative to baseline",
    fill = "OWF deployment scenario"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    legend.direction = "vertical"
  ) +
  # Add significance stars for specific facets
  geom_text(
    data = subset(LFI_catch_all, period == "2023-2034" & regulation == "no closure during operational phase"),
    aes(x = 1, y = 0.16, label = "*"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(LFI_catch_all, period == "2023-2034" & regulation == "no closure during operational phase"),
    aes(x = 4, y = 0.16, label = "*"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(LFI_catch_all, period == "2023-2034" & regulation == "trawlers closure during operational phase"),
    aes(x = 1, y = 0.16, label = "*"),
    inherit.aes = FALSE, size = 4
  )

# Tag facets with letters for easier referencing
tagged_facet <- tag_facet(combined_boxplot, 
                          open = "(", close = ")", tag_pool = letters, 
                          x = Inf, y = -Inf, 
                          hjust = 1.5, vjust = -1, 
                          fontface = "plain")

final_plot <- tagged_facet + theme(strip.text = element_text())

# Save final plot
ggsave(
  file.path("figures", "publication", "boxplot", "final_version","LFI_catch_revision.tiff"),
  final_plot,
  width = 6.69, height = 7,  # 双栏宽度（英寸）
  dpi = 500
)


