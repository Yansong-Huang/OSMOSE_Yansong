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
library(egg)

# variables globales
deployment_scenarios <- c("cout","protection","loin","equilibre")
regulation_scenarios <- c("sans_fermeture","fermeture_chalut","fermeture_totale")
CC_scenarios <- c("ON","OFF")
n_years_cut <- c(10,21,22,34,35,49)
n_replicate <- 30
total_biomass_base <- c(1322196,1334356,1334056)

process_biomass <- function(current_results_path,cut_off_year_begin, cut_off_year_end) {
  list_biomass_current <- list.files(current_results_path, "Yansong_biomass_Simu.*csv", full.names = TRUE)
  
  biomass_current_list <- lapply(1:n_replicate, function(simulation) {
    biomass_brut_current <- read.csv(list_biomass_current[simulation], skip = 1)
    biomass_total_current <- biomass_brut_current %>% 
      filter(Time>cut_off_year_begin)%>%
      filter(Time<cut_off_year_end)%>% 
      colMeans() %>% sum()
  })
  biomass_current <- as.vector(biomass_current_list) %>% as.numeric()
  return(biomass_current)
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

  # 分别计算三个时间段的数据
  
  total_biomass_during_list <- map(results_path_scenario, ~ process_biomass(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[3],
    cut_off_year_end = n_years_cut[4]
  ))
  
  total_biomass_after_list <- map(results_path_scenario, ~ process_biomass(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[5],
    cut_off_year_end = n_years_cut[6]
  ))
  
  names(total_biomass_during_list) <- c("cost", "protection", "distance", "balance")
  names(total_biomass_after_list) <- c("cost", "protection", "distance", "balance")
  
  # 转换为数据框并添加标识
  total_biomass_during_table <- stack(total_biomass_during_list) %>%
    mutate(values = values/total_biomass_base[2],period = "2023-2034", regulation = regulation)
  total_biomass_after_table <- stack(total_biomass_after_list) %>%
    mutate(values = values/total_biomass_base[3],period = "2035-2050", regulation = regulation)
  
  # 合并到全局数据框
  total_biomass_all <- rbind(
    total_biomass_all,
    total_biomass_during_table,
    total_biomass_after_table
  )
}
colnames(total_biomass_all) <- c("biomass_ratio", "deployment","period","regulation")

total_biomass_all$regulation <- factor(
  total_biomass_all$regulation,
  levels = c("sans_fermeture", "fermeture_chalut", "fermeture_totale"),
  labels = c("no closure during operational phase", "trawlers closure during operational phase", "complete closure during operational phase")
)


combined_boxplot <- ggplot(total_biomass_all, aes(x = deployment, y = biomass_ratio-1, fill = deployment)) +
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
    # title = "Total biomass across scenarios and periods, relative to reference simulations",
    x = "Deployment Scenario",
    y = "Total biomass relative to reference simulations",
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

tagged_facet <- tag_facet(combined_boxplot, 
          open = "(", close = ")", tag_pool = letters, 
          x = Inf, y = -Inf, 
          hjust = 1.5, vjust = -1, 
          fontface = "plain")

final_plot <- tagged_facet + theme(strip.text = element_text())

ggsave(
  file.path("figures", "publication", "boxplot", "total_biomass_revision.png"),
  final_plot,
  width = 12, height = 6, dpi = 600
)
