# Boxplot of mean weight by species
# Auteur : Yansong Huang
# Date de création : 2024-05-09

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
n_years_cut <- c(10,21,22,34,35,49)
n_species <- 16
n_replicate <- 30


# 构建场景路径
results_path_base <- file.path("outputs/results_1111","Base_simu","Base", "output", "CIEM")
results_path_scenario <- file.path("outputs/results_1111", "CC.ON_cout_sans_fermeture","Base", "output", "CIEM")

process_weight <- function(current_results_path, cut_off_year_begin, cut_off_year_end) {
  list_biomass_current <- list.files(current_results_path, "Yansong_biomass_Simu.*csv", full.names = TRUE)
  list_abundance_current <- list.files(current_results_path, "Yansong_abundance_Simu.*csv", full.names = TRUE)
  
  species_list <- c("lesserSpottedDogfish", "redMullet", "pouting", "whiting", "poorCod",
                    "cod", "dragonet", "sole", "plaice", "horseMackerel", 
                    "mackerel", "herring", "sardine", "squids", "cuttlefish", "thornbackRay")
  
  weight_summary <- bind_rows(lapply(1:n_species, function(species) {
    # 每个模拟重复计算一个时间段平均体重
    weight_all_simulations <- map_dfr(1:n_replicate, function(simulation) {
      biomass_brut <- read.csv(list_biomass_current[simulation], skip = 1)
      abundance_brut <- read.csv(list_abundance_current[simulation], skip = 1)
      
      biomass_period <- biomass_brut %>%
        filter(Time >= cut_off_year_begin, Time <= cut_off_year_end) %>%
        pull(species + 1)
      
      abundance_period <- abundance_brut %>%
        filter(Time >= cut_off_year_begin, Time <= cut_off_year_end) %>%
        pull(species + 1)
      
      # 计算吨→公斤，并取时间段平均体重
      weight_current <- ifelse(abundance_period > 0, biomass_period / abundance_period * 1000, NA)
      mean_weight <- mean(weight_current, na.rm = TRUE)
      
      data.frame(
        species_name = species_list[species],
        simulation = simulation,
        mean_weight_kg = mean_weight
      )
    })
    
    weight_all_simulations
  }))
  
  return(weight_summary)
}



# 2023-2034  
  # 计算分时间段的数据
weight_base_during <- process_weight(
  current_results_path = results_path_base,
  cut_off_year_begin = n_years_cut[3],
  cut_off_year_end = n_years_cut[4]
)


weight_base_during_mean <- weight_base_during %>%
  group_by(species_name) %>%
  summarise(mean_weight_kg = mean(mean_weight_kg, na.rm = TRUE))


weight_during <- process_weight(
    current_results_path = results_path_scenario,
    cut_off_year_begin = n_years_cut[3],
    cut_off_year_end = n_years_cut[4]
  )

# 给对照组的 mean_weight_kg 改个名字避免混淆
weight_base_during_mean <- weight_base_during_mean %>%
  rename(base_mean_weight_kg = mean_weight_kg)

# 按 species_name 左连接对照组的平均值
relative_weight_during <- weight_during %>%
  left_join(weight_base_during_mean, by = "species_name") %>%
  mutate(relative_to_base = mean_weight_kg / base_mean_weight_kg) %>%
  mutate(period="2023-2034")

# 2035-2050
weight_base_after <- process_weight(
  current_results_path = results_path_base,
  cut_off_year_begin = n_years_cut[5],
  cut_off_year_end = n_years_cut[6]
)

weight_base_after_mean <- weight_base_after %>%
  group_by(species_name) %>%
  summarise(mean_weight_kg = mean(mean_weight_kg, na.rm = TRUE))

weight_after <- process_weight(
  current_results_path = results_path_scenario,
  cut_off_year_begin = n_years_cut[5],
  cut_off_year_end = n_years_cut[6]
)

# 给对照组的 mean_weight_kg 改个名字避免混淆
weight_base_after_mean <- weight_base_after_mean %>%
  rename(base_mean_weight_kg = mean_weight_kg)

# 按 species_name 左连接对照组的平均值
relative_weight_after <- weight_after %>%
  left_join(weight_base_after_mean, by = "species_name") %>%
  mutate(relative_to_base = mean_weight_kg / base_mean_weight_kg) %>%
  mutate(period="2035-2050")


# 初始化全局数据框
weight_all <- data.frame()
  # 合并到全局数据框
  weight_all <- rbind(
    weight_all,
    relative_weight_during,
    relative_weight_after
  )

  weight_boxplot <- ggplot(weight_all, aes(x = species_name, y = relative_to_base-1)) +
    # 添加须线
    stat_summary(
      fun.data = "median_hilow",
      geom = "errorbar",
      aes(ymin = ..ymin.., ymax = ..ymax..),
      width = 0.2,
      color = "black"
    ) +
    geom_boxplot(fill = "purple", varwidth = TRUE, outlier.shape = NA, linetype = "blank") +
    # 添加平均值线
    stat_summary(
      fun = mean,
      geom = "errorbar",
      aes(ymin = ..y.., ymax = ..y..),
      width = 0.75,
      color = "black"
    ) +
    geom_hline(yintercept = 0, color = "black", linetype = "dotted") +
    facet_grid(period ~ ., scales = "free_y", labeller = labeller(
      period = label_wrap_gen(20)))+
    labs(
      # title = "Total biomass across scenarios and periods, relative to reference simulations",
      x = "Species",
      y = "Mean weight relative to reference simulations",
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
 print(weight_boxplot)

 ggsave(
   file.path("figures", "publication", "boxplot", "mean_weight_by_species.png"),
   weight_boxplot,
   width = 8, height = 5, dpi = 600
 )
