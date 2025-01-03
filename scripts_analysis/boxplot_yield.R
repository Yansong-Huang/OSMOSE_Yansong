# Boxplot of total yield
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

# variables globales
deployment_scenarios <- c("cout","protection","loin","equilibre")
regulation_scenarios <- c("sans_fermeture","fermeture_chalut","fermeture_totale")
CC_scenarios <- c("ON","OFF")
n_years_cut <- c(10,21,22,34,35,49)
n_replicate <- 30
total_yield_base <- c(59051.26,58021.87,56385.46)

process_yield <- function(current_results_path,cut_off_year_begin, cut_off_year_end) {
  list_yield_current <- list.files(current_results_path, "Yansong_yield_Simu.*csv", full.names = TRUE)
  
  yield_current_list <- lapply(1:n_replicate, function(simulation) {
    yield_brut_current <- read.csv(list_yield_current[simulation], skip = 1)
    yield_total_current <- yield_brut_current %>% 
      filter(Time>cut_off_year_begin)%>%
      filter(Time<cut_off_year_end)%>% 
      colMeans() %>% sum()
  })
  yield_current <- as.vector(yield_current_list) %>% as.numeric()
  return(yield_current)
}

# 初始化全局数据框
total_yield_all <- data.frame()

# 遍历每个捕鱼政策情境
for (regulation in regulation_scenarios) {
  
  # 构建场景路径
  results_path_1 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[1], "_", regulation), "Base", "output", "CIEM")
  results_path_2 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[2], "_", regulation), "Base", "output", "CIEM")
  results_path_3 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[3], "_", regulation), "Base", "output", "CIEM")
  results_path_4 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[4], "_", regulation), "Base", "output", "CIEM")
  
  results_path_scenario <- list(results_path_1, results_path_2, results_path_3, results_path_4)
  
  # 分别计算两个时间段的数据
  
  total_yield_during_list <- map(results_path_scenario, ~ process_yield(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[3],
    cut_off_year_end = n_years_cut[4]
  ))
  
  total_yield_after_list <- map(results_path_scenario, ~ process_yield(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[5],
    cut_off_year_end = n_years_cut[6]
  ))
  
  names(total_yield_during_list) <- c("cost", "protection", "distance", "balance")
  names(total_yield_after_list) <- c("cost", "protection", "distance", "balance")
  
  # 转换为数据框并添加标识
  total_yield_during_table <- stack(total_yield_during_list) %>%
    mutate(values = values/total_yield_base[2],period = "2023-2034", regulation = regulation)
  total_yield_after_table <- stack(total_yield_after_list) %>%
    mutate(values = values/total_yield_base[3],period = "2035-2050", regulation = regulation)
  
  # 合并到全局数据框
  total_yield_all <- rbind(
    total_yield_all,
    total_yield_during_table,
    total_yield_after_table
  )
}
colnames(total_yield_all) <- c("yield_ratio", "deployment","period","regulation")

total_yield_all$regulation <- factor(
  total_yield_all$regulation,
  levels = c("sans_fermeture", "fermeture_chalut", "fermeture_totale"),
  labels = c("no closure", "trawlers closure", "complete closure")
)

# 绘制大图

# deployment_boxplot <- ggplot(total_yield_all, aes(x = deployment, y = yield_ratio, fill = deployment)) +
#   geom_boxplot() +
#   geom_hline(yintercept = 1, color = "black", linetype = "dotted") +
#   facet_grid(~period, scales = "free_y", labeller = labeller(
#     period = label_wrap_gen(20))) +
#   ylim(0.85,1.1)+
#   scale_fill_manual(
#     values = c("purple", "pink", "orange", "lightblue"),
#     labels = c(
#       "Cost minimisation", "Exclusion from environmental protection zones",
#       "Long distance from the coast", "Balance"
#     )
#   ) +
#   labs(
#     title = "Total yield across scenarios and periods, relative to reference simulations",
#     x = "Deployment Scenario",
#     y = "Total yield relative to reference simulations",
#     fill = "Deployment Scenario"
#   ) +
#   theme_bw() +
#   theme(
#     plot.title = element_text(size = 14, face = "bold"),
#     axis.title.x = element_blank(),
#     axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
#     axis.text.y = element_text(size = 10),
#     legend.title = element_text(size = 13),
#     legend.text = element_text(size = 11)
#   )+
#   geom_text(data = subset(total_yield_all, period == "2035-2050"), 
#             aes(x = 1, y = 1.1, label = "***"), 
#             inherit.aes = FALSE, size = 4)+
#   geom_text(data = subset(total_yield_all, period == "2035-2050"), 
#             aes(x = 2, y = 1.1, label = "***"), 
#             inherit.aes = FALSE, size = 4)+
# geom_text(data = subset(total_yield_all, period == "2035-2050"), 
#           aes(x = 3, y = 1.1, label = "***"), 
#           inherit.aes = FALSE, size = 4)+
# geom_text(data = subset(total_yield_all, period == "2035-2050"), 
#           aes(x = 4, y = 1.1, label = "***"), 
#           inherit.aes = FALSE, size = 4)
#  
# print(deployment_boxplot)
# # 保存图像
# ggsave(
#   file.path("figures", "publication", "boxplot", "total_yield_deployment_unpaired.png"),
#   deployment_boxplot,
#   width = 12, height = 4, dpi = 600
# )
# 
# regulation_boxplot <- ggplot(total_yield_all, aes(x = regulation, y = yield_ratio, fill = regulation)) +
#   geom_boxplot() +
#   geom_hline(yintercept = 1, color = "black", linetype = "dotted") +
#   facet_grid(~period, scales = "free_y", labeller = labeller(
#     period = label_wrap_gen(20))) +
#   ylim(0.85,1.1)+
#   scale_fill_manual(
#     values = c("brown", "forestgreen", "dodgerblue4"),
#     labels = c("no closure", "trawlers closure", "complete closure")
#   ) +
#   labs(
#     title = "Total yield across scenarios and periods, relative to reference simulations",
#     x = "Regulation Scenario",
#     y = "Total yield relative to reference simulations",
#     fill = "Regulation Scenario"
#   ) +
#   theme_bw() +
#   theme(
#     plot.title = element_text(size = 14, face = "bold"),
#     axis.title.x = element_blank(),
#     axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
#     axis.text.y = element_text(size = 10),
#     legend.title = element_text(size = 13),
#     legend.text = element_text(size = 11)
#   )+
#   geom_text(data = subset(total_yield_all, period == "2035-2050"), 
#             aes(x = 1, y = 1.1, label = "***"), 
#             inherit.aes = FALSE, size = 4)+
#   geom_text(data = subset(total_yield_all, period == "2035-2050"), 
#             aes(x = 2, y = 1.1, label = "***"), 
#             inherit.aes = FALSE, size = 4)+
#   geom_text(data = subset(total_yield_all, period == "2035-2050"), 
#             aes(x = 3, y = 1.1, label = "***"), 
#             inherit.aes = FALSE, size = 4)
# 
# print(regulation_boxplot)
# # 保存图像
# ggsave(
#   file.path("figures", "publication", "boxplot", "total_yield_regulation_unpaired.png"),
#   regulation_boxplot,
#   width = 9, height = 4, dpi = 600
# )

combined_boxplot <- ggplot(total_yield_all, aes(x = deployment, y = yield_ratio, fill = deployment)) +
  geom_boxplot(varwidth = TRUE, coef = 0, outlier.shape = NA, linetype = "blank") +  # 隐藏中位数线
  stat_summary(
    fun = mean,
    geom = "errorbar",
    aes(ymin = ..y.., ymax = ..y..),
    width = 0.75,
    color = "black"
  ) +
  geom_hline(yintercept = 1, color = "black", linetype = "dotted") +
  facet_grid(period ~ regulation, scales = "free_y", labeller = labeller(
    period = label_wrap_gen(20), regulation = label_wrap_gen(20)
  )) +
  scale_fill_manual(
    values = c("purple", "pink", "orange", "lightblue"),
    labels = c(
      "Cost minimisation", "Exclusion from environmental protection zones",
      "Long distance from the coast", "Balance"
    )
  ) +
  labs(
    title = "Total yield across scenarios and periods, relative to reference simulations",
    x = "Deployment Scenario",
    y = "Total yield relative to reference simulations",
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
    data = subset(total_yield_all, period == "2023-2034" & regulation == "no closure"),
    aes(x = 1, y = 1.1, label = "*"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_yield_all, period == "2035-2050" & regulation == "no closure"),
    aes(x = 1, y = 1.1, label = "***"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_yield_all, period == "2035-2050" & regulation == "no closure"),
    aes(x = 2, y = 1.1, label = "*"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_yield_all, period == "2035-2050" & regulation == "no closure"),
    aes(x = 3, y = 1.1, label = "*"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_yield_all, period == "2035-2050" & regulation == "no closure"),
    aes(x = 4, y = 1.1, label = "*"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_yield_all, period == "2035-2050" & regulation == "trawlers closure"),
    aes(x = 1, y = 1.1, label = "***"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_yield_all, period == "2035-2050" & regulation == "trawlers closure"),
    aes(x = 2, y = 1.1, label = "***"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_yield_all, period == "2035-2050" & regulation == "trawlers closure"),
    aes(x = 3, y = 1.1, label = "***"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_yield_all, period == "2035-2050" & regulation == "trawlers closure"),
    aes(x = 4, y = 1.1, label = "***"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_yield_all, period == "2035-2050" & regulation == "complete closure"),
    aes(x = 1, y = 1.1, label = "***"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_yield_all, period == "2035-2050" & regulation == "complete closure"),
    aes(x = 2, y = 1.1, label = "***"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_yield_all, period == "2035-2050" & regulation == "complete closure"),
    aes(x = 3, y = 1.1, label = "***"),
    inherit.aes = FALSE, size = 4
  ) +
  geom_text(
    data = subset(total_yield_all, period == "2035-2050" & regulation == "complete closure"),
    aes(x = 4, y = 1.1, label = "***"),
    inherit.aes = FALSE, size = 4
  ) 

  print(combined_boxplot)

# ggsave(
#   file.path("figures", "publication", "boxplot", "total_yield_combined_mean.png"),
#   combined_boxplot,
#   width = 12, height = 8, dpi = 600
# )
