# Boxplot of total biomass
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

process_biomass <- function(base_results_path, current_results_path,cut_off_year_begin, cut_off_year_end) {
  list_biomass_base <- list.files(base_results_path, "Yansong_biomass_Simu.*csv", full.names = TRUE)
  list_biomass_current <- list.files(current_results_path, "Yansong_biomass_Simu.*csv", full.names = TRUE)
  
  biomass_relative <- lapply(1:n_replicate, function(simulation) {
    biomass_brut_base <- read.csv(list_biomass_base[simulation], skip = 1)
    biomass_brut_current <- read.csv(list_biomass_current[simulation], skip = 1)
    biomass_total_base <- biomass_brut_base %>% 
      filter(Time>cut_off_year_begin)%>%
      filter(Time<cut_off_year_end)%>% 
      colMeans() %>% sum()
    biomass_total_current <- biomass_brut_current %>% 
      filter(Time>cut_off_year_begin)%>%
      filter(Time<cut_off_year_end)%>% 
      colMeans() %>% sum()
    biomass_ratio <- biomass_total_current/biomass_total_base
  })
  biomass_relative <- as.vector(biomass_relative) %>% as.numeric()
  return(biomass_relative)
}

# 初始化全局数据框
total_biomass_all <- data.frame()

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
  total_biomass_before_list <- map(results_path_scenario, ~ process_biomass(
    base_results_path = results_path_base,
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[1],
    cut_off_year_end = n_years_cut[2]
  ))
  
  total_biomass_during_list <- map(results_path_scenario, ~ process_biomass(
    base_results_path = results_path_base,
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[3],
    cut_off_year_end = n_years_cut[4]
  ))
  
  total_biomass_after_list <- map(results_path_scenario, ~ process_biomass(
    base_results_path = results_path_base,
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[5],
    cut_off_year_end = n_years_cut[6]
  ))
  
  names(total_biomass_before_list) <- c("cost", "protection", "distance", "balance")
  names(total_biomass_during_list) <- c("cost", "protection", "distance", "balance")
  names(total_biomass_after_list) <- c("cost", "protection", "distance", "balance")
  
  # 转换为数据框并添加标识
  total_biomass_before_table <- stack(total_biomass_before_list) %>%
    mutate(period = "2011-2022", regulation = regulation)
  total_biomass_during_table <- stack(total_biomass_during_list) %>%
    mutate(period = "2023-2034", regulation = regulation)
  total_biomass_after_table <- stack(total_biomass_after_list) %>%
    mutate(period = "2035-2050", regulation = regulation)
  
  # 合并到全局数据框
  total_biomass_all <- rbind(
    total_biomass_all,
    total_biomass_before_table,
    total_biomass_during_table,
    total_biomass_after_table
  )
}
colnames(total_biomass_all) <- c("biomass_ratio", "deployment","period","regulation")

total_biomass_all$regulation <- factor(
  total_biomass_all$regulation,
  levels = c("sans_fermeture", "fermeture_chalut", "fermeture_totale"),
  labels = c("no closure", "trawlers closure", "complete closure")
)

# 绘制大图
combined_boxplot <- ggplot(total_biomass_all, aes(x = deployment, y = biomass_ratio, fill = deployment)) +
  geom_boxplot() +
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
    title = "Total Biomass Across Regulations and Periods",
    x = "Deployment Scenario",
    y = "Relative Biomass",
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

# 保存图像
ggsave(
  file.path("figures", "publication", "boxplot", "total_biomass_combined.png"),
  combined_boxplot,
  width = 15, height = 10, dpi = 600
)
