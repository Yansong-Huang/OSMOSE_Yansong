# Séries temporelles de capture totale
# Auteur : Yansong Huang
# Date de création : 2024-10-22

library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(purrr)
library(ncdf4)

# variables globales
deployment_scenarios <- c("cout","protection","loin","equilibre")
regulation_scenarios <- c("sans_fermeture","fermeture_chalut","fermeture_totale")
year_begin <- 2002
year_begin_proj <- 2022
year_end <- 2050
n_years <- 49
cut_off_year <- 10 
n_replicate <- 30
deployment_colour_palette <- c("#8a2be2","#ff1493","#ff3800","#0892d0")

process_yield_ratio <- function(current_results_path) {
  list_yield_scenario <- list.files(current_results_path, "Yansong_yield_Simu.*csv", full.names = TRUE)
  
  yield_sum_replicates <- map_dfc(1:n_replicate, function(simulation) {
    yield_brut_scenario <- read.csv(list_yield_scenario[simulation], skip = 1)
    yield_sum_scenario <- apply(yield_brut_scenario[cut_off_year:n_years,-1],1,sum) # yield of all species combined
    yield_sum_ratio <- yield_sum_scenario/yield_base_mean
  }) 
  
  yield_summary <-  data.frame(
    year = 2011:2050,
    yield_output_mean = rowMeans(yield_sum_replicates),
    yield_output_sd = apply(yield_sum_replicates, 1, sd)
  )
  return(yield_summary)
}

# calculate reference data 
results_path_1 <- file.path("outputs/results_1111","Base_simu","Base","output","CIEM")
list_yield_reference <- list.files(results_path_1, "Yansong_yield_Simu.*csv", full.names = TRUE)

yield_base_replicates <- map_dfc(1:n_replicate, function(simulation) {
  yield_brut_reference <- read.csv(list_yield_reference[simulation], skip = 1)
  yield_sum_reference <- apply(yield_brut_reference[cut_off_year:n_years,-1],1,sum) # yield of all species combined
}) 

yield_base_mean = rowMeans(yield_base_replicates)

# Prepare the data for all regulation scenarios
all_yield_ratio_df <- list()  # Initialize an empty list to store all results
for (regulation in regulation_scenarios) {
  # Define the paths for each scenario
  results_path_2 <- file.path("outputs/results_1111",paste0("CC.ON_",deployment_scenarios[1],"_",regulation),"Base","output","CIEM")
  results_path_3 <- file.path("outputs/results_1111",paste0("CC.ON_",deployment_scenarios[2],"_",regulation),"Base","output","CIEM")
  results_path_4 <- file.path("outputs/results_1111",paste0("CC.ON_",deployment_scenarios[3],"_",regulation),"Base","output","CIEM")
  results_path_5 <- file.path("outputs/results_1111",paste0("CC.ON_",deployment_scenarios[4],"_",regulation),"Base","output","CIEM")
  
  # List of paths for processing
  scenario_path <- list(results_path_2, results_path_3, results_path_4, results_path_5)
  
  # Process yield ratio for all scenarios
  all_yield_ratio <- lapply(scenario_path, process_yield_ratio)
  
  # Combine the results into one dataframe, including deployment scenario and regulation
  all_yield_ratio_df[[regulation]] <- bind_rows(
    lapply(1:length(all_yield_ratio), function(i) {
      df <- all_yield_ratio[[i]]
      df$deployment <- deployment_scenarios[i]
      df$regulation <- regulation
      return(df)
    })
  )
}

# After the loop, we have all yield data for each regulation scenario
# Combine all data into one dataframe
combined_yield_ratio_df <- bind_rows(all_yield_ratio_df)
combined_yield_ratio_df$regulation <- factor(
  combined_yield_ratio_df$regulation,
  levels = c("sans_fermeture", "fermeture_chalut", "fermeture_totale"),
  labels = c("no closure", "trawlers closure", "complete closure")
)
combined_yield_ratio_df$deployment <- factor(
  combined_yield_ratio_df$deployment,
  levels = c("cout", "protection", "loin", "equilibre"),
  labels = c("Cost minisation", "Exclusion from environmental protection zones",
             "Long distance from the coast", "Balance")
)

combined_plot <- ggplot(combined_yield_ratio_df) +
  geom_line(aes(x = year, y = yield_output_mean, color = deployment)) +
  geom_ribbon(aes(x = year, ymin = yield_output_mean - yield_output_sd, ymax = yield_output_mean + yield_output_sd, fill = deployment), alpha = 0.2) +
  geom_hline(yintercept = 1, color = "black", linetype = "dotted") +
  annotate("rect", xmin = 2023, xmax = 2025, ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.5) +
  annotate("rect", xmin = 2028, xmax = 2030, ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.5) +
  annotate("rect", xmin = 2033, xmax = 2035, ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.5) +
  scale_color_manual(name = "Deployment Scenario", values = deployment_colour_palette) +
  scale_fill_manual(name = "Deployment Scenario", values = deployment_colour_palette) +
  ylab("Yield Ratio") +
  facet_wrap(~ regulation, ncol = 1) +  # 纵向排列子图
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())

# 保存合并后的图像
ggsave("figures/publication/time_series/combined_yield.png", combined_plot, width = 8, height = 8, dpi = 600)
