# Boxplot of mean yield by species
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

process_yield <- function(current_results_path, cut_off_year_begin, cut_off_year_end) {
  list_yield_current <- list.files(current_results_path, "Yansong_yield_Simu.*csv", full.names = TRUE)
  
  species_list <- c("lesserSpottedDogfish", "redMullet", "pouting", "whiting", "poorCod",
                    "cod", "dragonet", "sole", "plaice", "horseMackerel", 
                    "mackerel", "herring", "sardine", "squids", "cuttlefish", "thornbackRay")
  
  yield_summary <- bind_rows(lapply(1:n_species, function(species) {
    # 每个模拟重复计算一个时间段平均
    yield_all_simulations <- map_dfr(1:n_replicate, function(simulation) {
      yield_brut <- read.csv(list_yield_current[simulation], skip = 1)
      
      yield_period <- yield_brut %>%
        filter(Time >= cut_off_year_begin, Time <= cut_off_year_end) %>%
        pull(species + 1)
      
      # 取时间段平均
      mean_yield <- mean(yield_period, na.rm = TRUE)
      
      data.frame(
        species_name = species_list[species],
        simulation = simulation,
        mean_yield = mean_yield
      )
    })
    
    yield_all_simulations
  }))
  
  return(yield_summary)
}



# 2023-2034  
# 计算分时间段的数据
yield_base_during <- process_yield(
  current_results_path = results_path_base,
  cut_off_year_begin = n_years_cut[3],
  cut_off_year_end = n_years_cut[4]
)


yield_base_during_mean <- yield_base_during %>%
  group_by(species_name) %>%
  summarise(mean_yield = mean(mean_yield, na.rm = TRUE))


yield_during <- process_yield(
  current_results_path = results_path_scenario,
  cut_off_year_begin = n_years_cut[3],
  cut_off_year_end = n_years_cut[4]
)

# 给对照组的 mean_yield 改个名字避免混淆
yield_base_during_mean <- yield_base_during_mean %>%
  rename(base_mean_yield = mean_yield)

# 按 species_name 左连接对照组的平均值
relative_yield_during <- yield_during %>%
  left_join(yield_base_during_mean, by = "species_name") %>%
  mutate(relative_to_base = mean_yield / base_mean_yield) %>%
  mutate(period="2023-2034")

# 2035-2050
yield_base_after <- process_yield(
  current_results_path = results_path_base,
  cut_off_year_begin = n_years_cut[5],
  cut_off_year_end = n_years_cut[6]
)

yield_base_after_mean <- yield_base_after %>%
  group_by(species_name) %>%
  summarise(mean_yield = mean(mean_yield, na.rm = TRUE))

yield_after <- process_yield(
  current_results_path = results_path_scenario,
  cut_off_year_begin = n_years_cut[5],
  cut_off_year_end = n_years_cut[6]
)

# 给对照组的 mean_yield 改个名字避免混淆
yield_base_after_mean <- yield_base_after_mean %>%
  rename(base_mean_yield = mean_yield)

# 按 species_name 左连接对照组的平均值
relative_yield_after <- yield_after %>%
  left_join(yield_base_after_mean, by = "species_name") %>%
  mutate(relative_to_base = mean_yield / base_mean_yield) %>%
  mutate(period="2035-2050")


# 初始化全局数据框
yield_all <- data.frame()
# 合并到全局数据框
yield_all <- rbind(
  yield_all,
  relative_yield_during,
  relative_yield_after
)

yield_all <- yield_all %>%
  filter(species_name != "cod")

yield_boxplot <- ggplot(yield_all, aes(x = species_name, y = relative_to_base-1)) +
  # 添加须线
  stat_summary(
    fun.data = "median_hilow",
    geom = "errorbar",
    aes(ymin = ..ymin.., ymax = ..ymax..),
    width = 0.2,
    color = "black"
  ) +
  geom_boxplot(fill = "darkgreen", varwidth = TRUE, outlier.shape = NA, linetype = "blank") +
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
  coord_cartesian(ylim = c(-1, 2)) + 
  labs(
    # title = "Total yield across scenarios and periods, relative to reference simulations",
    x = "Species",
    y = "yield change relative to reference simulations",
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
print(yield_boxplot)

ggsave(
  file.path("figures", "publication", "boxplot", "yield_by_species.png"),
  yield_boxplot,
  width = 8, height = 5, dpi = 600
)

###### absolute yield under cost minimisation*no closure scenario

scenario_yield_boxplot <- ggplot(yield_all, aes(x = species_name, y = mean_yield)) +
  # 添加须线
  stat_summary(
    fun.data = "median_hilow",
    geom = "errorbar",
    aes(ymin = ..ymin.., ymax = ..ymax..),
    width = 0.2,
    color = "black"
  ) +
  geom_boxplot(fill = "darkgreen", varwidth = TRUE, outlier.shape = NA, linetype = "blank") +
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
    # title = "Total yield across scenarios and periods, relative to reference simulations",
    x = "Species",
    y = "Scenario yield (t)",
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
print(scenario_yield_boxplot)

ggsave(
  file.path("figures", "publication", "boxplot", "scenario_yield_by_species.png"),
  scenario_yield_boxplot,
  width = 8, height = 5, dpi = 600
)
