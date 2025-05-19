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

# 1. calculate mortality rates ----- 
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
    
    # 第一步：读取第一行，判断哪些列的标记是 "Mpred"
    header_row <- readr::read_csv(
      mortality_file,
      col_names = FALSE,
      skip = 1,
      n_max = 1,
      col_types = readr::cols(.default = "c"),
      show_col_types = FALSE
    ) %>%
      as.character()
    
    # 找出列索引：哪些列在第一行中是 "Mpred"
    keep_cols_index <- which(header_row %in% mortality_source)
    
    # Step 2: 直接读取指定列的数据（跳过第一行，不使用列名）
    original_data <- readr::read_csv(
      mortality_file,
      skip = 1,
      col_names = FALSE,
      col_select = all_of(keep_cols_index),
      show_col_types = FALSE
    )
    
    # data transform
    mortality <- original_data %>%
      t() %>%
      data.frame()
    
    colnames(mortality) <- c("source", "stage", 1:n_years_simu)
    
    mortality_long <- mortality %>%
      pivot_longer(cols = 3:(n_years_simu + 2), names_to = "Year", values_to = "mortality") %>%
      mutate(mortality = as.numeric(mortality),
             Year = as.numeric(Year)) %>%
      filter(Year >= cut_off_year_begin, Year <= cut_off_year_end)
    

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

# 2. visualisation ====
# 相对值
mortality_all_sources <- readRDS("indicators/Mpred_Mstarv_F_pre_recruits_recruits.rds") 
mortality_focus <- mortality_all_sources %>%
  filter(species_name %in% c("cuttlefish", "herring", "redMullet")) %>%
  filter(period=="2035-2050") %>%
  filter(source %in% c("F","Mpred"))

mortality_focus[mortality_focus == "F"] <- "Fishing"
mortality_focus[mortality_focus == "Mpred"] <- "Predation"



mortality_plot_all <- ggplot(mortality_focus, aes(x = species_name, y = relative_to_base-1)) +
  geom_boxplot(fill = "lightblue", varwidth = TRUE, outlier.shape = NA, linetype = "blank") +
  # 添加须线
  stat_summary(
    fun.data = "median_hilow",
    geom = "errorbar",
    aes(ymin = ..ymin.., ymax = ..ymax..),
    width = 0.2,
    color = "black"
  ) +
  geom_boxplot(fill = "lightblue", varwidth = TRUE, outlier.shape = NA, linetype = "blank") +
  # 添加平均值线
  stat_summary(fun = mean, geom = "errorbar",
    aes(ymin = ..y.., ymax = ..y..), width = 0.75, color = "black") +
  geom_hline(yintercept = 0, color = "black", linetype = "dotted") +
  facet_grid(~source, scales = "free_y") +
  # coord_cartesian(ylim = c(-1, 1)) + 
  labs(
    # title = "Mortality rates by source and species",
    x = "Species",
    y = "Mortality change"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10)
  )

print(mortality_plot_all)

ggsave(
  file.path("figures", "publication", "boxplot", "mortality_change_focus_species.png"),
  mortality_plot_all,
  width = 8, height = 4, dpi = 600
)

# 绝对值
mortality_plot_scenario <- ggplot(mortality_all_sources, aes(x = species_name, y = mean_mortality)) +
  geom_boxplot(fill = "lightblue", varwidth = TRUE, outlier.shape = NA) +
  # stat_summary(fun = mean, geom = "errorbar",
  # aes(ymin = ..y.., ymax = ..y..), width = 0.75, color = "black") +
  facet_grid(source ~ period, scales = "free_y") +
  labs(
    # title = "Mortality rates by source and species",
    x = "Species",
    y = "Test scenario Mortality"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10)
  )

print(mortality_plot_scenario)

ggsave(
  file.path("figures", "publication", "boxplot", "scenario_mortality_significant_species_2.png"),
  mortality_plot_scenario,
  width = 8, height = 6, dpi = 600
)

