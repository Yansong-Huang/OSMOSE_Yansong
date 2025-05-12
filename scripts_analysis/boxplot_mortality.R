# Boxplot of mortality by species
# Auteur : Yansong Huang
# Date de création : 2024-05-09

library(ggplot2)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(purrr)
library(ncdf4)
library(patchwork)
library(egg)
library(stringr)

# variables globales
n_years_simu <- 49
n_years_cut <- c(10,21,22,34,35,49)
n_species <- 16


# 构建场景路径
results_path_base <- file.path("outputs/results_1111","Base_simu","Base", "output", "CIEM")
results_path_scenario <- file.path("outputs/results_1111", "CC.ON_cout_sans_fermeture","Base", "output", "CIEM")


process_mortality <- function(current_results_path, cut_off_year_begin, cut_off_year_end, 
                              mortality_source = NULL, mortality_stage = NULL) {
  mortality_path <- file.path(current_results_path, "Mortality")
  list_mortality <- list.files(mortality_path, "Yansong_mortalityRate-[a-zA-Z]+_Simu[0-9]+\\.csv$", full.names = TRUE)

  mortality_summary <- map_dfr(seq_along(list_mortality), function(i) {
       
    mortality_file <- list_mortality[i]
    # 从文件路径中提取 species 和 replicate
    species <- str_match(mortality_file, "mortalityRate-(.*?)_Simu")[,2]
    replicate <- as.integer(str_match(mortality_file, "_Simu(\\d+)\\.csv")[,2])
    
    original_data <- readr::read_csv(mortality_file, col_names = FALSE, skip = 1)
    
    mortality <- original_data %>%
      select(-X1) %>%
      t() %>%
      data.frame()
    
    colnames(mortality) <- c("source", "stage", 1:n_years_simu)
    
    mortality_long <- mortality %>%
      pivot_longer(cols = 3:(n_years_simu + 2), names_to = "Year", values_to = "mortality") %>%
      mutate(mortality = as.numeric(mortality),
             Year = as.numeric(Year)) %>%
      filter(Year >= cut_off_year_begin, Year <= cut_off_year_end)
    
    # 如果指定了 source，就筛选
    # if (!is.null(mortality_source) && length(mortality_source) > 0 && any(nzchar(mortality_source))) {
      mortality_long <- mortality_long %>%
        filter(source %in% mortality_source)
    # }

    # 如果指定了 stage，就筛选
    # if (!is.null(mortality_stage)) {
      mortality_long <- mortality_long %>%
        filter(stage %in% mortality_stage)
    # }
    
    mean_mortality <- mean(mortality_long$mortality, na.rm = TRUE)
    
    data.frame(
      species_name = species,
      simulation = replicate,
      mean_mortality = mean_mortality
    )
  })
  
  return(mortality_summary)
}
# ==== 设定阶段筛选条件 ====
# stage_filter <- c("Eggs", "Pre-recruits", "Recruits")
stage_filter <- c("Pre-recruits", "Recruits")

# ==== 所有死亡来源 ====
source_list <- c("Mpred", "Mstarv","F")

# ==== 初始化总数据框 ====
mortality_all_sources <- data.frame()

# ==== 遍历每个死亡来源 ====
for (source_filter in source_list) {
  # --- 时间段 2023-2034 ---
  mortality_during_base <- process_mortality(results_path_base, n_years_cut[3], n_years_cut[4],
                                             mortality_source = source_filter, mortality_stage = stage_filter)
  
  mortality_during_base_mean <- mortality_during_base %>%
    group_by(species_name) %>%
    summarise(mean_mortality = mean(mean_mortality, na.rm = TRUE))
  
  mortality_during <- process_mortality(results_path_scenario, n_years_cut[3], n_years_cut[4],
                                        mortality_source = source_filter, mortality_stage = stage_filter)
  
  # 给对照组mean_mortality改个名字base_mean_mortality避免混淆
  mortality_during_base_mean <- mortality_during_base_mean %>%
    rename(base_mean_mortality = mean_mortality)
  
  
  relative_mortality_during <- mortality_during %>%
    left_join(mortality_during_base_mean, by = "species_name") %>%
    mutate(relative_to_base = mean_mortality / base_mean_mortality,
           period = "2023-2034", source = source_filter)
  
  # --- 时间段 2035-2050 ---
  mortality_after_base <- process_mortality(results_path_base, n_years_cut[5], n_years_cut[6],
                                            mortality_source = source_filter, mortality_stage = stage_filter)
  
  mortality_after_base_mean <- mortality_after_base %>%
    group_by(species_name) %>%
    summarise(mean_mortality = mean(mean_mortality, na.rm = TRUE))
  
  mortality_after <- process_mortality(results_path_scenario, n_years_cut[5], n_years_cut[6],
                                       mortality_source = source_filter, mortality_stage = stage_filter)
  
  # 给对照组mean_mortality改个名字base_mean_mortality避免混淆
  mortality_after_base_mean <- mortality_after_base_mean %>%
    rename(base_mean_mortality = mean_mortality)
  
  relative_mortality_after <- mortality_after %>%
    left_join(mortality_after_base_mean, by = "species_name") %>%
    mutate(relative_to_base = mean_mortality / base_mean_mortality,
           period = "2035-2050", source = source_filter)
  
  # 合并当前source下所有数据
  mortality_all_sources <- bind_rows(mortality_all_sources, relative_mortality_during, relative_mortality_after)
}

mortality_all_sources <- mortality_all_sources %>%
  filter(species_name != "cod")

# ==== 可视化 ====
mortality_plot_all <- ggplot(mortality_all_sources, aes(x = species_name, y = relative_to_base-1)) +
  geom_boxplot(fill = "lightblue", varwidth = TRUE, outlier.shape = NA) +
  # stat_summary(fun = mean, geom = "errorbar",
               # aes(ymin = ..y.., ymax = ..y..), width = 0.75, color = "black") +
  facet_grid(source ~ period, scales = "free_y") +
  coord_cartesian(ylim = c(-1, 1)) + 
  labs(
    title = "Mortality rates by source and species",
    x = "Species",
    y = "Relative mortality to reference simulations"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10)
  )

print(mortality_plot_all)

# ==== 保存 ====
ggsave(
  file.path("figures", "publication", "boxplot", "mortality_change_by_source_and_species.png"),
  mortality_plot_all,
  width = 12, height = 8, dpi = 600
)

mortality_plot_base <- ggplot(mortality_all_sources, aes(x = species_name, y = base_mean_mortality)) +
  geom_boxplot(fill = "lightblue", varwidth = TRUE, outlier.shape = NA) +
  # stat_summary(fun = mean, geom = "errorbar",
  # aes(ymin = ..y.., ymax = ..y..), width = 0.75, color = "black") +
  facet_grid(source ~ period, scales = "free_y") +
  labs(
    title = "Mortality rates by source and species",
    x = "Species",
    y = "Base mortality"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10)
  )

print(mortality_plot_base)

# ==== 保存 ====
ggsave(
  file.path("figures", "publication", "boxplot", "base_mortality_by_source_and_species.png"),
  mortality_plot_base,
  width = 12, height = 8, dpi = 600
)

