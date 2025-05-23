# Boxplot of LFI catch 40
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
n_years_cut <- c(10,21,22,34,35,49)
n_replicate <- 30
LFI_catch_base <- c(0.1255017,0.1032469,0.1388709)

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


process_LFI <- function(current_results_path, cut_off_year_begin, cut_off_year_end) {
  # 获取文件列表
  list_yield_current <- list.files(current_results_path, "Yansong_yieldDistribBySize_Simu.*csv", full.names = TRUE)
  
  # 计算LFI
  lfi_relative <- lapply(1:n_replicate, function(simulation) {
    # 读取数据
    yield_brut_current <- read.csv(list_yield_current[simulation], skip = 1)
    
    # 筛选时间段
    yield_current_filtered <- yield_brut_current %>% filter(Time > cut_off_year_begin & Time < cut_off_year_end)
    
    # 计算LFI
    lfi_current <- LFI(yield_current_filtered)
    
    return(lfi_current)
  })
  
  # 转换为数值向量
  lfi_relative <- as.numeric(lfi_relative)
  return(lfi_relative)
}

# 初始化全局数据框
LFI_catch_all <- data.frame()

# 遍历每个捕鱼政策情境
for (regulation in regulation_scenarios) {
  
  # 构建每个场景的路径
  results_path_1 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[1], "_", regulation), "Base", "output", "CIEM","SizeIndicators")
  results_path_2 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[2], "_", regulation), "Base", "output", "CIEM","SizeIndicators")
  results_path_3 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[3], "_", regulation), "Base", "output", "CIEM","SizeIndicators")
  results_path_4 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[4], "_", regulation), "Base", "output", "CIEM","SizeIndicators")
  
  # 将路径合并为列表
  results_path_scenario <- list(results_path_1, results_path_2, results_path_3, results_path_4)
  
  # 分别计算两个时间段的数据
  
  LFI_catch_during_list <- map(results_path_scenario, ~ process_LFI(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[3],
    cut_off_year_end = n_years_cut[4]
  ))
  
  LFI_catch_period_list <- map(results_path_scenario, ~ process_LFI(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[5],
    cut_off_year_end = n_years_cut[6]
  ))
  
  names(LFI_catch_during_list) <- c("cost", "protection", "distance", "balance")
  names(LFI_catch_period_list) <- c("cost", "protection", "distance", "balance")
  
  # 转换为数据框并添加标识
  LFI_catch_during_table <- stack(LFI_catch_during_list) %>%
    mutate(values = values/LFI_catch_base[2],period = "2023-2034", regulation = regulation)
  LFI_catch_period_table <- stack(LFI_catch_period_list) %>%
    mutate(values = values/LFI_catch_base[3],period = "2035-2050", regulation = regulation)
  
  # 合并到全局数据框
  LFI_catch_all <- rbind(
    LFI_catch_all,
    LFI_catch_during_table,
    LFI_catch_period_table
  )
}
colnames(LFI_catch_all) <- c("LFI_ratio", "deployment","period","regulation")

LFI_catch_all$regulation <- factor(
  LFI_catch_all$regulation,
  levels = c("sans_fermeture", "fermeture_chalut", "fermeture_totale"),
  labels = c("no closure during operational phase", "trawlers closure during operational phase", "complete closure during operational phase")
)

# 绘制大图
combined_boxplot <- ggplot(LFI_catch_all, aes(x = deployment, y = LFI_ratio-1, fill = deployment)) +
  # 添加须线
  stat_summary(
    fun.data = "median_hilow",
    geom = "errorbar",
    aes(ymin = ..ymin.., ymax = ..ymax..),
    width = 0.2,
    color = "black"
  ) +
  geom_boxplot(varwidth = TRUE, outlier.shape = NA, linetype = "blank") +
  # 添加平均值线
  stat_summary(
    fun = mean,
    geom = "errorbar",
    aes(ymin = ..y.., ymax = ..y..),
    width = 0.75,
    color = "black"
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "dotted") +
  facet_grid(period ~ regulation, scales = "free_y", labeller = labeller(
    period = label_wrap_gen(20), regulation = label_wrap_gen(25)
  )) +
  scale_fill_manual(
    values = c("purple", "pink", "orange", "lightblue"),
    labels = c(
      "Cost minimisation", "Exclusion from environmental protection zones",
      "Long distance from the coast", "Balance"
    )
  ) +
  labs(
    # title = "LFI catch across scenarios and periods, relative to reference simulations",
    x = "Deployment Scenario",
    y = "LFI catch relative to reference simulations",
    fill = "Deployment Scenario"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11)
  ) +
  # 为特定分面单独定义星号数据
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

tagged_facet <- tag_facet(combined_boxplot, 
                          open = "(", close = ")", tag_pool = letters, 
                          x = Inf, y = -Inf, 
                          hjust = 1.5, vjust = -1, 
                          fontface = "plain")

final_plot <- tagged_facet + theme(strip.text = element_text())

ggsave(
  file.path("figures", "publication", "boxplot", "LFI_catch_revision.png"),
  final_plot,
  width = 12, height = 6, dpi = 600
)


# subset_condition <- LFI_catch_all$deployment == "balance" & 
#   LFI_catch_all$regulation == "no closure during operational phase" &
#   LFI_catch_all$period == "2023-2034"
# 
# selected_LFI_ratio <- LFI_catch_all$LFI_ratio[subset_condition]
# mean(selected_LFI_ratio)


