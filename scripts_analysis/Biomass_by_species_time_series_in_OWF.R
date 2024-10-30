# Calculs des biomasse par espèce au sein des parcs éoliens
# Auteur : Yansong Huang
# Date de création : 2024-08-14

library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(purrr)
library(ncdf4)

source("scripts_analysis/OWF_mask.R")
# variable globales
deployment_scenarios <- c("cout","protection","loin","equilibre")
regulation_scenarios <- c("sans_fermeture","fermeture_chalut","fermeture_totale")
CC_scenarios <- c("ON","OFF")
year_begin <- 2002
year_begin_proj <- 2022
year_end <- 2050
n_years <- 49
cut_off_year <- 9 # commence la visualisation de l'année 2010
n_replicate <- 30
n_species <- 16
biomass_sd_colour_palette <- c("#8a2be2","#ff1493","#ff3800","#0892d0")
biomass_mean_colour_palette <- c("#8a2be2","#8a2be2","#ff1493","#ff1493","#ff3800","#ff3800","#0892d0","#0892d0")

# chemin pour tous les résultats
regulation <- regulation_scenarios[2]
results_path_1 <- file.path("outputs/results_2510","Base_simu","output","CIEM")
results_path_2 <- file.path("outputs/results_2510",paste0("CC.",CC_scenarios[1],"_",deployment_scenarios[1],"_",regulation),"Base","output","CIEM")
results_path_3 <- file.path("outputs/results_2510",paste0("CC.",CC_scenarios[1],"_",deployment_scenarios[2],"_",regulation),"Base","output","CIEM")
results_path_4 <- file.path("outputs/results_2510",paste0("CC.",CC_scenarios[1],"_",deployment_scenarios[3],"_",regulation),"Base","output","CIEM")
results_path_5 <- file.path("outputs/results_2510",paste0("CC.",CC_scenarios[1],"_",deployment_scenarios[4],"_",regulation),"Base","output","CIEM")

scenario_path <- list(results_path_2, results_path_3, results_path_4, results_path_5)


###### biomass ######

# Chargement les résultats de sortie du modèle
all_biomass <- list()

process_biomass <- function(current_results_path) {
  # define nc file list for spatialized biomass
  list_biomass_nc_base <- list.files(results_path_1, "Yansong_spatializedBiomass_Simu.", full.names = TRUE)
  list_biomass_nc_current <- list.files(current_results_path, "Yansong_spatializedBiomass_Simu.", full.names = TRUE)

  # exclude cod from the analysis
  biomass_relative <- bind_rows(lapply(c(1:5,7:16), function(species) {
    # non-spatialized biomass
    biomass_species <- map_dfc(1:10, function(simulation) {
      # biomass in OWF
      # open nc files
      biomass_nc_base <- nc_open(list_biomass_nc_base[simulation])
      biomass_nc_current <- nc_open(list_biomass_nc_current[simulation])
      #read variable
      biomass_spatial_base <- ncvar_get(biomass_nc_base,"Biomass")
      biomass_spatial_current <- ncvar_get(biomass_nc_current,"Biomass")
      nc_close(biomass_nc_base)
      nc_close(biomass_nc_current)

      # cut from 2011-2050
      biomass_species_base <- biomass_spatial_base[,,species,cut_off_year:n_years]
      biomass_species_current <- biomass_spatial_current[,,species,cut_off_year:n_years]

      # list for spatialized biomass ratio,
      OWF_biomass_base_list <- list()
      OWF_biomass_current_list <- list()

      for (longitude in 1:45) {
        for (latitude in 1:22) {
          if (!is.na(biomass_species_base[longitude, latitude, 1])) {
            if (mask_OWF[longitude, latitude]) {
              OWF_biomass_base_list[[length(OWF_biomass_base_list) + 1]] <- biomass_species_base[longitude, latitude, ]
              OWF_biomass_current_list[[length(OWF_biomass_current_list) + 1]] <- biomass_species_current[longitude, latitude, ]
            }
          }
        }
      }
      # combine biomass in all OWF cells
      OWF_biomass_spatial_base <- do.call(cbind, OWF_biomass_base_list)
      OWF_biomass_spatial_current <- do.call(cbind, OWF_biomass_current_list)
      OWF_biomass_base <- rowMeans(OWF_biomass_spatial_base)
      OWF_biomass_current <- rowMeans(OWF_biomass_spatial_current)

      OWF_biomass_ratio <- OWF_biomass_current/OWF_biomass_base
      return(OWF_biomass_ratio)
    })

    species_list <- c("lesserSpottedDogfish", "redMullet", "pouting", "whiting", "poorCod", "cod", "dragonet", "sole", "plaice", "horseMackerel", "mackerel", "herring", "sardine", "squids", "cuttlefish", "thornbackRay")
    biomass_ratio <- data.frame(
      year = 2010:2050,
      species_name = species_list[species],
      biomass_output_mean = rowMeans(biomass_species),
      biomass_output_sd = apply(biomass_species, 1, sd)
    )
  }))

  return(biomass_relative)
}

# apply to all scenarios
all_biomass <- lapply(scenario_path, process_biomass)

# Création du graphique de comparaison

biomass_plot <- ggplot() +
  geom_line(data = all_biomass[[1]], aes(x = year, y = biomass_output_mean, color = "mean cost")) +
  geom_ribbon(data = all_biomass[[1]], aes(x = year,
                                           ymin = biomass_output_mean - biomass_output_sd,
                                           ymax = biomass_output_mean + biomass_output_sd,
                                           fill = "sd cost"),
              alpha = 0.2) +
  geom_line(data = all_biomass[[2]], aes(x = year, y = biomass_output_mean, color = "mean protection")) +
  geom_ribbon(data = all_biomass[[2]], aes(x = year,
                                           ymin = biomass_output_mean - biomass_output_sd,
                                           ymax = biomass_output_mean + biomass_output_sd,
                                           fill = "sd protection"),
              alpha = 0.2) +
  geom_line(data = all_biomass[[3]], aes(x = year, y = biomass_output_mean, color = "mean distance")) +
  geom_ribbon(data = all_biomass[[3]], aes(x = year,
                                           ymin = biomass_output_mean - biomass_output_sd,
                                           ymax = biomass_output_mean + biomass_output_sd,
                                           fill = "sd distance"),
              alpha = 0.2) +
  geom_line(data = all_biomass[[4]], aes(x = year, y = biomass_output_mean, color = "mean balance")) +
  geom_ribbon(data = all_biomass[[4]], aes(x = year,
                                           ymin = biomass_output_mean - biomass_output_sd,
                                           ymax = biomass_output_mean + biomass_output_sd,
                                           fill = "sd balance"),
              alpha = 0.2) +
  geom_hline(yintercept = 1, color = "black", linetype = "dotted")+
  annotate("rect", xmin = 2023, xmax = 2025, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "grey")+
  annotate("rect", xmin = 2028, xmax = 2030, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "grey")+
  annotate("rect", xmin = 2033, xmax = 2035, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "grey")+
  scale_color_manual(name = element_blank(),
                     values = biomass_mean_colour_palette,
                     breaks = c("mean cost","sd cost","mean protection","sd protection","mean distance","sd distance", "mean balance", "sd balance"),
                     labels = c("mean cost","sd cost","mean protection","sd protection","mean distance","sd distance", "mean balance", "sd balance")) +
  scale_fill_manual(name = element_blank(),
                    values = biomass_sd_colour_palette,
                    breaks = c("sd cost","sd protection","sd distance","sd balance"),
                    labels = c("sd cost","sd protection","sd distance","sd balance")) +
  facet_wrap(~species_name, scales = "free_y", ncol = 4) +
  ylab("biomass ratio") +
  ggtitle(paste("biomasse par espèce en OWF relative à la simulation référente, scénario",regulation))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.title = element_blank())

print(biomass_plot)
# Sauvegarder le graphique de comparaison
ggsave(file.path("figures/publication/time_series", regulation, "OWF_biomass_by_species_2510.png"), biomass_plot, width = 15, height = 8, dpi = 600)

###### yield ######

# # Chargement les résultats de sortie du modèle
# all_yield <- list()
# 
# process_yield <- function(current_results_path) {
#   # define nc file list for spatialized yield
#   results_path_base<- file.path("results_tl","Base_simu","output","CIEM")
#   list_yield_nc_base <- list.files(results_path_base, "Yansong_spatializedYieldBiomass_Simu.", full.names = TRUE)
#   list_yield_nc_current <- list.files(current_results_path, "Yansong_spatializedYieldBiomass_Simu.", full.names = TRUE)
#   
#   # exclude cod from the analysis
#   yield_relative <- bind_rows(lapply(c(1:4,8:16), function(species) {
#     # non-spatialized yield
#     yield_species <- map_dfc(1:10, function(simulation) {
#       # yield in OWF
#       # open nc files
#       yield_nc_base <- nc_open(list_yield_nc_base[simulation])
#       yield_nc_current <- nc_open(list_yield_nc_current[simulation])
#       #read variable
#       yield_spatial_base <- ncvar_get(yield_nc_base,"YieldBiomass")
#       yield_spatial_current <- ncvar_get(yield_nc_current,"YieldBiomass")
#       nc_close(yield_nc_base)
#       nc_close(yield_nc_current)
#       
#       # cut from 2011-2050
#       yield_species_base <- yield_spatial_base[,,species,n_years_cut:n_years]
#       yield_species_current <- yield_spatial_current[,,species,n_years_cut:n_years]
#       
#       # list for spatialized yield ratio,
#       OWF_yield_base_list <- list()
#       OWF_yield_current_list <- list()
#       
#       for (longitude in 1:45) {
#         for (latitude in 1:22) {
#           if (!is.na(yield_species_base[longitude, latitude, 1])) {
#             if (mask_OWF[longitude, latitude]) {
#               OWF_yield_base_list[[length(OWF_yield_base_list) + 1]] <- yield_species_base[longitude, latitude, ]
#               OWF_yield_current_list[[length(OWF_yield_current_list) + 1]] <- yield_species_current[longitude, latitude, ]
#             }
#           }
#         }
#       }
#       # combine yield in all OWF cells 
#       OWF_yield_spatial_base <- do.call(cbind, OWF_yield_base_list)
#       OWF_yield_spatial_current <- do.call(cbind, OWF_yield_current_list)
#       OWF_yield_base <- rowMeans(OWF_yield_spatial_base)
#       OWF_yield_current <- rowMeans(OWF_yield_spatial_current)
#       
#       OWF_yield_ratio <- OWF_yield_current/OWF_yield_base
#       return(OWF_yield_ratio)
#     })
#     
#     species_list <- c("lesserSpottedDogfish", "redMullet", "pouting", "whiting", "poorCod", "cod", "dragonet", "sole", "plaice", "horseMackerel", "mackerel", "herring", "sardine", "squids", "cuttlefish", "thornbackRay")
#     yield_ratio <- data.frame(
#       year = year_begin_cut:year_end,
#       species_name = species_list[species],
#       yield_ratio_mean = rowMeans(yield_species),
#       yield_ratio_sd = apply(yield_species, 1, sd)
#     )
#   }))
#   
#   return(yield_relative)
# }
# 
# # apply to all scenarios
# all_yield <- lapply(results_path, process_yield)
# # Création du graphique de comparaison
# 
# mean_colour_palette <- c("brown",brewer.pal(12, "Paired"))
# sd_colour_palette <- mean_colour_palette[c(1,3,5,7,9,11,13,15)]
# 
# yield_comparison_plot <- ggplot() +
#   geom_line(data = all_yield[[1]], aes(x = year, y = yield_ratio_mean, color = "mean no closure")) +
#   geom_ribbon(data = all_yield[[1]], aes(x = year,
#                                            ymin = yield_ratio_mean - yield_ratio_sd,
#                                            ymax = yield_ratio_mean + yield_ratio_sd,
#                                            fill = "sd no closure"),
#               alpha = 0.2) +
#   geom_line(data = all_yield[[2]], aes(x = year, y = yield_ratio_mean, color = "mean trawlers closure")) +
#   geom_ribbon(data = all_yield[[2]], aes(x = year,
#                                            ymin = yield_ratio_mean - yield_ratio_sd,
#                                            ymax = yield_ratio_mean + yield_ratio_sd,
#                                            fill = "sd trawlers closure"),
#               alpha = 0.2) +
#   geom_line(data = all_yield[[3]], aes(x = year, y = yield_ratio_mean, color = "mean complete closure")) +
#   geom_ribbon(data = all_yield[[3]], aes(x = year,
#                                            ymin = yield_ratio_mean - yield_ratio_sd,
#                                            ymax = yield_ratio_mean + yield_ratio_sd,
#                                            fill = "sd complete closure"),
#               alpha = 0.2) +
#   geom_hline(yintercept = 1, color = "black", linetype = "dotted")+
#   scale_color_manual(name = element_blank(),
#                      values = mean_colour_palette,
#                      breaks = c("mean no closure","sd no closure","mean trawlers closure","sd trawlers closure","mean complete closure","sd complete closure"),
#                      labels = c("mean no closure","sd no closure","mean trawlers closure","sd trawlers closure","mean complete closure","sd complete closure")) +
#   scale_fill_manual(name = element_blank(),
#                     values = sd_colour_palette,
#                     breaks = c("sd no closure","sd trawlers closure","sd complete closure"),
#                     labels = c("sd no closure","sd trawlers closure","sd complete closure")) +
#   facet_wrap(~species_name, scales = "free_y", ncol = 4) +  # Specify ncol parameter as 4
#   geom_hline(yintercept = 1, color = "black")+
#   geom_vline(xintercept = 2023, color = "black", linetype = "dotted")+
#   geom_vline(xintercept = 2028, color = "black", linetype = "dotted")+
#   geom_vline(xintercept = 2033, color = "black", linetype = "dotted")+
#   ylab("relative yield change") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.background = element_rect(fill = "white"),
#         legend.title = element_blank())
# 
# print(yield_comparison_plot)
# # Sauvegarder le graphique de comparaison
# ggsave(file.path("figures_CIEM_CC_old", deployment, "OWF_relative_yield_all_regulations.png"), yield_comparison_plot, width = 15, height = 8, dpi = 600)
