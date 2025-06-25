# Boxplot of mortality by species
# Author: Yansong Huang
# Creation date: 2025-05-09

library(ggplot2)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(purrr)
library(ncdf4)
library(patchwork)
library(egg)
library(stringr)

# 1. Calculate mortality rates -----

# Global variables
n_years_simu <- 49  # Total number of years simulated
n_years_cut <- c(10,21,22,34,35,49)  # Cut-off years to define time periods
n_species <- 16  # Number of species

# Define scenario paths
results_path_base <- file.path("outputs/results_1111","Base_simu","Base", "output", "CIEM")
results_path_scenario <- file.path("outputs/results_1111", "CC.ON_cout_sans_fermeture","Base", "output", "CIEM")

# Function to calculate mean mortality per species and simulation for a given source and life stage
process_mortality <- function(current_results_path, cut_off_year_begin, cut_off_year_end, 
                              mortality_source = NULL, mortality_stage = NULL) {
  mortality_path <- file.path(current_results_path, "Mortality")
  list_mortality <- list.files(mortality_path, "Yansong_mortalityRate-[a-zA-Z]+_Simu[0-9]+\\.csv$", full.names = TRUE)
  
  mortality_summary <- map_dfr(seq_along(list_mortality), function(i) {
    mortality_file <- list_mortality[i]
    species <- str_match(mortality_file, "mortalityRate-(.*?)_Simu")[,2]  # Extract species
    replicate <- as.integer(str_match(mortality_file, "_Simu(\\d+)\\.csv")[,2])  # Extract simulation number
    
    # Read the second line (actual header info about source type)
    header_row <- readr::read_csv(
      mortality_file,
      col_names = FALSE,
      skip = 1,
      n_max = 1,
      col_types = readr::cols(.default = "c"),
      show_col_types = FALSE
    ) %>%
      as.character()
    
    keep_cols_index <- which(header_row %in% mortality_source)  # Get column indices matching source
    
    # Read mortality values only for the selected columns
    original_data <- readr::read_csv(
      mortality_file,
      skip = 1,
      col_names = FALSE,
      col_select = all_of(keep_cols_index),
      show_col_types = FALSE
    )
    
    # Transpose and convert to dataframe
    mortality <- original_data %>%
      t() %>%
      data.frame()
    
    colnames(mortality) <- c("source", "stage", 1:n_years_simu)  # Assign column names
    
    # Reshape to long format
    mortality_long <- mortality %>%
      pivot_longer(cols = 3:(n_years_simu + 2), names_to = "Year", values_to = "mortality") %>%
      mutate(mortality = as.numeric(mortality), Year = as.numeric(Year)) %>%
      filter(Year >= cut_off_year_begin, Year <= cut_off_year_end)
    
    mortality_long <- mortality_long %>%
      filter(stage %in% mortality_stage)  # Filter by life stage
    
    mean_mortality <- mean(mortality_long$mortality, na.rm = TRUE)
    
    data.frame(
      species_name = species,
      simulation = replicate,
      mean_mortality = mean_mortality
    )
  })
  
  return(mortality_summary)
}

# ==== Filter conditions for life stages ====
stage_filter <- c("Pre-recruits", "Recruits")

# ==== List of mortality sources to analyze ====
source_list <- c("Mpred", "Mstarv","F")

# ==== Initialize result dataframe ====
mortality_all_sources <- data.frame()

# ==== Loop over mortality sources and periods ====
for (source_filter in source_list) {
  # --- Period: 2023-2034 ---
  mortality_during_base <- process_mortality(results_path_base, n_years_cut[3], n_years_cut[4],
                                             mortality_source = source_filter, mortality_stage = stage_filter)
  mortality_during_base_mean <- mortality_during_base %>%
    group_by(species_name) %>%
    summarise(mean_mortality = mean(mean_mortality, na.rm = TRUE))
  
  mortality_during <- process_mortality(results_path_scenario, n_years_cut[3], n_years_cut[4],
                                        mortality_source = source_filter, mortality_stage = stage_filter)
  mortality_during_base_mean <- mortality_during_base_mean %>%
    rename(base_mean_mortality = mean_mortality)
  
  relative_mortality_during <- mortality_during %>%
    left_join(mortality_during_base_mean, by = "species_name") %>%
    mutate(relative_to_base = mean_mortality / base_mean_mortality,
           period = "2023-2034", source = source_filter)
  
  # --- Period: 2035-2050 ---
  mortality_after_base <- process_mortality(results_path_base, n_years_cut[5], n_years_cut[6],
                                            mortality_source = source_filter, mortality_stage = stage_filter)
  mortality_after_base_mean <- mortality_after_base %>%
    group_by(species_name) %>%
    summarise(mean_mortality = mean(mean_mortality, na.rm = TRUE))
  
  mortality_after <- process_mortality(results_path_scenario, n_years_cut[5], n_years_cut[6],
                                       mortality_source = source_filter, mortality_stage = stage_filter)
  mortality_after_base_mean <- mortality_after_base_mean %>%
    rename(base_mean_mortality = mean_mortality)
  
  relative_mortality_after <- mortality_after %>%
    left_join(mortality_after_base_mean, by = "species_name") %>%
    mutate(relative_to_base = mean_mortality / base_mean_mortality,
           period = "2035-2050", source = source_filter)
  
  # Combine into main dataframe
  mortality_all_sources <- bind_rows(mortality_all_sources, relative_mortality_during, relative_mortality_after)
}

# Remove species if needed
mortality_all_sources <- mortality_all_sources %>%
  filter(species_name != "cod")

# 2. Visualisation ====

# Load mortality data from saved RDS if available (optional)
mortality_all_sources <- readRDS("indicators/Mpred_Mstarv_F_pre_recruits_recruits.rds") 

# Filter data to selected species and sources
mortality_focus <- mortality_all_sources %>%
  filter(species_name %in% c("cuttlefish", "herring", "redMullet")) %>%
  filter(period=="2035-2050") %>%
  filter(source %in% c("F","Mpred"))

# Replace codes with human-readable labels
mortality_focus[mortality_focus == "F"] <- "Fishing"
mortality_focus[mortality_focus == "Mpred"] <- "Predation"

# Plot relative mortality changes (boxplots)
mortality_plot_all <- ggplot(mortality_focus, aes(x = species_name, y = relative_to_base-1)) +
  geom_boxplot(fill = "lightblue", varwidth = TRUE, outlier.shape = NA, linetype = "blank") +
  stat_summary(
    fun.data = "median_hilow",
    geom = "errorbar",
    aes(ymin = ..ymin.., ymax = ..ymax..),
    width = 0.2,
    color = "black"
  ) +
  geom_boxplot(fill = "lightblue", varwidth = TRUE, outlier.shape = NA, linetype = "blank") +
  stat_summary(fun = mean, geom = "errorbar",
               aes(ymin = ..y.., ymax = ..y..), width = 0.75, color = "black") +
  geom_hline(yintercept = 0, color = "black", linetype = "dotted") +
  facet_grid(~source, scales = "free_y") +
  labs(x = "Species", y = "Mortality change") +
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

# Absolute mortality values for scenario
mortality_plot_scenario <- ggplot(mortality_all_sources, aes(x = species_name, y = mean_mortality)) +
  geom_boxplot(fill = "lightblue", varwidth = TRUE, outlier.shape = NA) +
  facet_grid(source ~ period, scales = "free_y") +
  labs(x = "Species", y = "Test scenario Mortality") +
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
