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

process_yield <- function(current_results_path,cut_off_year_begin, cut_off_year_end) {
  list_yield_base <- list.files(results_path_base, "Yansong_yield_Simu.*csv", full.names = TRUE)
  list_yield_current <- list.files(current_results_path, "Yansong_yield_Simu.*csv", full.names = TRUE)
  
  yield_relative <- lapply(1:n_replicate, function(simulation) {
    yield_brut_base <- read.csv(list_yield_base[simulation], skip = 1)
    yield_brut_current <- read.csv(list_yield_current[simulation], skip = 1)
    yield_total_base <- yield_brut_base %>% 
      filter(Time>cut_off_year_begin)%>%
      filter(Time<cut_off_year_end)%>% 
      colMeans() %>% sum()
    yield_total_current <- yield_brut_current %>% 
      filter(Time>cut_off_year_begin)%>%
      filter(Time<cut_off_year_end)%>% 
      colMeans() %>% sum()
    yield_ratio <- yield_total_current/yield_total_base
  })
  yield_relative <- as.vector(yield_relative) %>% as.numeric()
  return(yield_relative)
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
  results_path_base <- file.path("outputs/results_1111", "Base_simu", "Base", "output", "CIEM")
  
  # 分别计算三个时间段的数据
  total_yield_before_list <- map(results_path_scenario, ~ process_yield(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[1],
    cut_off_year_end = n_years_cut[2]
  ))
  
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
  
  names(total_yield_before_list) <- c("cost", "protection", "distance", "balance")
  names(total_yield_during_list) <- c("cost", "protection", "distance", "balance")
  names(total_yield_after_list) <- c("cost", "protection", "distance", "balance")
  
  # 转换为数据框并添加标识
  total_yield_before_table <- stack(total_yield_before_list) %>%
    mutate(period = "2011-2022", regulation = regulation)
  total_yield_during_table <- stack(total_yield_during_list) %>%
    mutate(period = "2023-2034", regulation = regulation)
  total_yield_after_table <- stack(total_yield_after_list) %>%
    mutate(period = "2035-2050", regulation = regulation)
  
  # 合并到全局数据框
  total_yield_all <- rbind(
    total_yield_all,
    total_yield_before_table,
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

deployment_boxplot <- ggplot(total_yield_all, aes(x = deployment, y = yield_ratio, fill = deployment)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, color = "black", linetype = "dotted") +
  facet_grid(~period, scales = "free_y", labeller = labeller(
    period = label_wrap_gen(20))) +
  ylim(0.7,1.2)+
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
  )

print(deployment_boxplot)
# 保存图像
ggsave(
  file.path("figures", "publication", "boxplot", "total_yield_deployment.png"),
  deployment_boxplot,
  width = 12, height = 4, dpi = 600
)

regulation_boxplot <- ggplot(total_yield_all, aes(x = regulation, y = yield_ratio, fill = regulation)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, color = "black", linetype = "dotted") +
  facet_grid(~period, scales = "free_y", labeller = labeller(
    period = label_wrap_gen(20))) +
  ylim(0.7,1.2)+
  scale_fill_manual(
    values = c("darkred", "darkgreen", "darkblue"),
    labels = c("no closure", "trawlers closure", "complete closure")
  ) +
  labs(
    title = "Total yield across scenarios and periods, relative to reference simulations",
    x = "Regulation Scenario",
    y = "Total yield relative to reference simulations",
    fill = "Regulation Scenario"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11)
  )

print(regulation_boxplot)
# 保存图像
ggsave(
  file.path("figures", "publication", "boxplot", "total_yield_regulation.png"),
  regulation_boxplot,
  width = 9, height = 4, dpi = 600
)



