# Séries temporelles de biomasse totale
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
CC_scenarios <- c("ON","OFF")
year_begin <- 2002
year_begin_proj <- 2022
year_end <- 2050
n_years <- 49
cut_off_year <- 9 # commence la visualisation de l'année 2010
n_replicate <- 10
biomass_sd_colour_palette <- c("#8a2be2","#ff1493","#ff3800","#0892d0")
biomass_mean_colour_palette <- c("#8a2be2","#8a2be2","#ff1493","#ff1493","#ff3800","#ff3800","#0892d0","#0892d0")

# chemin pour tous les résultats
regulation <- regulation_scenarios[3]
results_path_1 <- file.path("outputs/results_spatial","Base_simu","output","CIEM")
results_path_2 <- file.path("outputs/results_spatial",paste0("CC.",CC_scenarios[1],"_",deployment_scenarios[1],"_",regulation),"Base","output","CIEM")
results_path_3 <- file.path("outputs/results_spatial",paste0("CC.",CC_scenarios[1],"_",deployment_scenarios[2],"_",regulation),"Base","output","CIEM")
results_path_4 <- file.path("outputs/results_spatial",paste0("CC.",CC_scenarios[1],"_",deployment_scenarios[3],"_",regulation),"Base","output","CIEM")
results_path_5 <- file.path("outputs/results_spatial",paste0("CC.",CC_scenarios[1],"_",deployment_scenarios[4],"_",regulation),"Base","output","CIEM")

scenario_path <- list(results_path_2, results_path_3, results_path_4, results_path_5)

# Chargement les résultats de sortie du modèle
# all_biomass <- list()
#
# process_biomass <- function(current_results_path) {
#   list_biomass <- list.files(current_results_path, "Yansong_biomass_Simu.", full.names = TRUE)
#
#   biomass_sum_replicates <- map_dfc(1:n_replicate, function(simulation) {
#       biomass_brut <- read.csv(list_biomass[simulation], skip = 1)
#       biomass_sum <- apply(biomass_brut[9:n_years,-1],1,sum) # biomass of all species combined
#    })
#     # species_list <- c("lesserSpottedDogfish", "redMullet", "pouting", "whiting", "poorCod", "cod", "dragonet", "sole", "plaice", "horseMackerel", "mackerel", "herring", "sardine", "squids", "cuttlefish", "thornbackRay")
#   biomass_summary <-  data.frame(
#       year = 2010:2050,
#       biomass_output_mean = rowMeans(biomass_sum_replicates),
#       biomass_output_sd = apply(biomass_sum_replicates, 1, sd)
#     )
#   return(biomass_summary)
# }

# # apply to all scenarios
# all_biomass <- lapply(results_path, process_biomass)
#
# # time series of biomass by species
#
# biomass_comparison_plot <- ggplot() +
#   geom_line(data = all_biomass[[1]], aes(x = year, y = biomass_output_mean, color = "mean control")) +
#   geom_ribbon(data = all_biomass[[1]], aes(x = year,
#                                            ymin = biomass_output_mean - biomass_output_sd,
#                                            ymax = biomass_output_mean + biomass_output_sd,
#                                            fill = "sd control"),
#               alpha = 0.2) +
#   geom_line(data = all_biomass[[2]], aes(x = year, y = biomass_output_mean, color = "mean cost")) +
#   geom_ribbon(data = all_biomass[[2]], aes(x = year,
#                                            ymin = biomass_output_mean - biomass_output_sd,
#                                            ymax = biomass_output_mean + biomass_output_sd,
#                                            fill = "sd cost"),
#               alpha = 0.2) +
#   geom_line(data = all_biomass[[3]], aes(x = year, y = biomass_output_mean, color = "mean protection")) +
#   geom_ribbon(data = all_biomass[[3]], aes(x = year,
#                                            ymin = biomass_output_mean - biomass_output_sd,
#                                            ymax = biomass_output_mean + biomass_output_sd,
#                                            fill = "sd protection"),
#               alpha = 0.2) +
#   geom_line(data = all_biomass[[4]], aes(x = year, y = biomass_output_mean, color = "mean distance")) +
#   geom_ribbon(data = all_biomass[[4]], aes(x = year,
#                                            ymin = biomass_output_mean - biomass_output_sd,
#                                            ymax = biomass_output_mean + biomass_output_sd,
#                                            fill = "sd distance"),
#               alpha = 0.2) +
#   geom_line(data = all_biomass[[5]], aes(x = year, y = biomass_output_mean, color = "mean balance")) +
#   geom_ribbon(data = all_biomass[[5]], aes(x = year,
#                                            ymin = biomass_output_mean - biomass_output_sd,
#                                            ymax = biomass_output_mean + biomass_output_sd,
#                                            fill = "sd balance"),
#               alpha = 0.2) +
#   scale_color_manual(name = element_blank(),
#                      values = biomass_mean_colour_palette,
#                      breaks = c("mean control","sd control","mean cost","sd cost","mean protection","sd protection","mean distance","sd distance", "mean balance", "sd balance"),
#                      labels = c("mean control","sd control","mean cost","sd cost","mean protection","sd protection","mean distance","sd distance", "mean balance", "sd balance")) +
#   scale_fill_manual(name = element_blank(),
#                     values = biomass_sd_colour_palette,
#                     breaks = c("sd control","sd cost","sd protection","sd distance","sd balance"),
#                     labels = c("sd control","sd cost","sd protection","sd distance","sd balance")) +
#   ylab("biomass (t)") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.background = element_rect(fill = "white"),
#         legend.title = element_blank())
#
# print(biomass_comparison_plot)
# # Sauvegarder le graphique de comparaison
# ggsave(file.path("figures/publication",regulation, "total_biomass_time_series.png", sep=""), biomass_comparison_plot, width = 15, height = 8, dpi = 600)

# time series of relative total biomass


# Chargement les résultats de sortie du modèle
all_biomass_ratio <- list()

process_biomass_ratio <- function(current_results_path) {
  list_biomass_scenario <- list.files(current_results_path, "Yansong_biomass_Simu.", full.names = TRUE)
  list_biomass_reference <- list.files(results_path_1, "Yansong_biomass_Simu.", full.names = TRUE)
  
  biomass_sum_replicates <- map_dfc(1:n_replicate, function(simulation) {
    biomass_brut_scenario <- read.csv(list_biomass_scenario[simulation], skip = 1)
    biomass_brut_reference <- read.csv(list_biomass_reference[simulation], skip = 1)
    biomass_sum_scenario <- apply(biomass_brut_scenario[cut_off_year:n_years,-1],1,sum) # biomass of all species combined
    biomass_sum_reference <- apply(biomass_brut_reference[cut_off_year:n_years,-1],1,sum) # biomass of all species combined
    biomass_sum_ratio <- biomass_sum_scenario/biomass_sum_reference
  }) 
  # species_list <- c("lesserSpottedDogfish", "redMullet", "pouting", "whiting", "poorCod", "cod", "dragonet", "sole", "plaice", "horseMackerel", "mackerel", "herring", "sardine", "squids", "cuttlefish", "thornbackRay")
  biomass_summary <-  data.frame(
    year = 2010:2050,
    biomass_output_mean = rowMeans(biomass_sum_replicates),
    biomass_output_sd = apply(biomass_sum_replicates, 1, sd)
  )
  return(biomass_summary)
}

# apply to all scenarios
all_biomass_ratio <- lapply(scenario_path, process_biomass_ratio)

relative_biomass_plot <- ggplot() +
  geom_line(data = all_biomass_ratio[[1]], aes(x = year, y = biomass_output_mean, color = "mean cost")) +
  geom_ribbon(data = all_biomass_ratio[[1]], aes(x = year,
                                           ymin = biomass_output_mean - biomass_output_sd,
                                           ymax = biomass_output_mean + biomass_output_sd,
                                           fill = "sd cost"),
              alpha = 0.2) +
  geom_line(data = all_biomass_ratio[[2]], aes(x = year, y = biomass_output_mean, color = "mean protection")) +
  geom_ribbon(data = all_biomass_ratio[[2]], aes(x = year,
                                           ymin = biomass_output_mean - biomass_output_sd,
                                           ymax = biomass_output_mean + biomass_output_sd,
                                           fill = "sd protection"),
              alpha = 0.2) +
  geom_line(data = all_biomass_ratio[[3]], aes(x = year, y = biomass_output_mean, color = "mean distance")) +
  geom_ribbon(data = all_biomass_ratio[[3]], aes(x = year,
                                           ymin = biomass_output_mean - biomass_output_sd,
                                           ymax = biomass_output_mean + biomass_output_sd,
                                           fill = "sd distance"),
              alpha = 0.2) +
  geom_line(data = all_biomass_ratio[[4]], aes(x = year, y = biomass_output_mean, color = "mean balance")) +
  geom_ribbon(data = all_biomass_ratio[[4]], aes(x = year,
                                           ymin = biomass_output_mean - biomass_output_sd,
                                           ymax = biomass_output_mean + biomass_output_sd,
                                           fill = "sd balance"),
              alpha = 0.2) +
  geom_hline(yintercept = 1, color = "black", linetype = "dotted")+
  geom_rect(aes(xmin = 2023, xmax = 2025, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.5) +
  geom_rect(aes(xmin = 2028, xmax = 2030, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.5) +
  geom_rect(aes(xmin = 2033, xmax = 2035, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.5) +
  scale_color_manual(name = element_blank(),
                     values = biomass_mean_colour_palette,
                     breaks = c("mean cost","sd cost","mean protection","sd protection","mean distance","sd distance", "mean balance", "sd balance"),
                     labels = c("mean cost","sd cost","mean protection","sd protection","mean distance","sd distance", "mean balance", "sd balance")) +
  scale_fill_manual(name = element_blank(),
                    values = biomass_sd_colour_palette,
                    breaks = c("sd cost","sd protection","sd distance","sd balance"),
                    labels = c("sd cost","sd protection","sd distance","sd balance")) +
  ylab("biomasse ratio") +
  ggtitle(paste("biomasse relative à la simulation référente, scénario",regulation))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.title = element_blank())

print(relative_biomass_plot)
# Sauvegarder le graphique de comparaison
ggsave(file.path("figures/publication/time_series",regulation,"total_biomass.png", sep=""), relative_biomass_plot, width = 15, height = 8, dpi = 600)

