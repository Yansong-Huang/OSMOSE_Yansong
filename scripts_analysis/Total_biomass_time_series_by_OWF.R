# Calculs des biomasse totale au sein de chaque parc éolien
# Auteur : Yansong Huang
# Date de création : 2024-08-14

library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(purrr)
library(ncdf4)
source("scripts_analysis/OWF_locations.R")
# variable globales
deployment_scenarios <- c("cout","protection","loin","equilibre")
regulation_scenarios <- c("sans_fermeture","fermeture_chalut","fermeture_totale")
CC_scenarios <- c("ON","OFF")
year_begin <- 2002
# year_begin_proj <- 2022
year_end <- 2050
n_years <- 49
# cut_off_year <- 9 # commence la visualisation de l'année 2010
n_replicate <- 30
n_species <- 16

OWF_names <- c("Calvados", "Fecamp", "Dieppe", "Dunkerque", "Centre Manche 1", "Centre Manche 2", "Rampion", "Future OWF")
repeated_OWF_names <- rep(OWF_names, each = 2)
colour_palette <- c(rep("darkred",4),rep("darkgreen",3),rep("darkblue",2))


# chemin pour tous les résultats
regulation <- regulation_scenarios[1]
results_path_base <- file.path("outputs/results_2510","Base_simu","output","CIEM")
results_path_OWF <- file.path("outputs/results_2510",paste0("CC.",CC_scenarios[1],"_",deployment_scenarios[4],"_",regulation),"Base","output","CIEM")

# Chargement les résultats de sortie du modèle
relative_biomass_summary <- list()

process_biomass <- function(current_results_path) {
  # define nc file list
  list_biomass_nc_base <- list.files(results_path_base, "Yansong_spatializedBiomass_Simu.", full.names = TRUE)
  list_biomass_nc_current <- list.files(current_results_path, "Yansong_spatializedBiomass_Simu.", full.names = TRUE)
  
  # define years range
  years <- year_begin:year_end
  n_years <- length(years)
  
  # a list for stocking results by OWF of all replicates
  relative_biomass_by_OWF_all_replicates <- list()
  
  for (replicate in 1:n_replicate) {
    # a temperal list for save results of each replicate
    relative_biomass_by_OWF_temp <- list()
    
    # open nc files
    biomass_nc_base <- nc_open(list_biomass_nc_base[replicate])
    biomass_nc_current <- nc_open(list_biomass_nc_current[replicate])
    # read biomass from nc files
    biomass_spatial_base <- ncvar_get(biomass_nc_base, "Biomass")
    biomass_spatial_current <- ncvar_get(biomass_nc_current, "Biomass")
    nc_close(biomass_nc_base)
    nc_close(biomass_nc_current)
    
    # loop over each OWF
    for (OWF_name in names(OWF_locations)) {
      lon_index <- OWF_locations[[OWF_name]]$lon
      lat_index <- OWF_locations[[OWF_name]]$lat
      biomass_spatial_OWF_base_list <- lapply(1:length(lon_index), function(i) {
        biomass_spatial_base[lon_index[i], lat_index[i], , ]
      })
      biomass_spatial_OWF_current_list <- lapply(1:length(lon_index), function(i) {
        biomass_spatial_current[lon_index[i], lat_index[i], , ]
      })
      
      biomass_spatial_OWF_base <- simplify2array(biomass_spatial_OWF_base_list)
      biomass_spatial_OWF_current <- simplify2array(biomass_spatial_OWF_current_list)
      # order des dimensions (n_species, length(lon_index), n_years)
      biomass_spatial_OWF_base <- aperm(biomass_spatial_OWF_base, perm = c(3, 1, 2))
      biomass_spatial_OWF_current <- aperm(biomass_spatial_OWF_current, perm = c(3, 1, 2))
      
      # aggregate all cells within the OWF
      total_biomass_in_OWF_base <- apply(biomass_spatial_OWF_base, 3, sum)
      total_biomass_in_OWF_current <- apply(biomass_spatial_OWF_current, 3, sum)
      
      # avoid dividing by 0
      total_biomass_in_OWF_base[total_biomass_in_OWF_base == 0] <- 1e-10
      relative_biomass_by_OWF_temp[[OWF_name]] <- total_biomass_in_OWF_current / total_biomass_in_OWF_base
    }
    
    # stock each replicate to the main list
    for (OWF_name in names(relative_biomass_by_OWF_temp)) {
      if (is.null(relative_biomass_by_OWF_all_replicates[[OWF_name]])) {
        relative_biomass_by_OWF_all_replicates[[OWF_name]] <- matrix(nrow = n_years, ncol = n_replicate)
      }
      relative_biomass_by_OWF_all_replicates[[OWF_name]][, replicate] <- relative_biomass_by_OWF_temp[[OWF_name]]
    }
  }
  
  # calculate mean and sd
  relative_biomass_by_OWF_summary <- list()
  for (OWF_name in names(relative_biomass_by_OWF_all_replicates)) {
    yearly_data <- relative_biomass_by_OWF_all_replicates[[OWF_name]]
    yearly_mean <- rowMeans(yearly_data)
    yearly_sd <- apply(yearly_data, 1, sd)
    relative_biomass_by_OWF_summary[[OWF_name]] <- data.frame(
      year = years,
      mean = yearly_mean,
      sd = yearly_sd
    )
  }
  
  return(relative_biomass_by_OWF_summary)
}
  
# relative_biomass_summary <- lapply(scenario_path, process_biomass)
relative_biomass_summary <- process_biomass(results_path_OWF)


# figure composée
relative_biomass_combined <- bind_rows(
  lapply(1:length(relative_biomass_summary), function(i) {
    relative_biomass_summary[[i]] %>%
      mutate(OWF = factor(OWF_names[i], levels = OWF_names))  # Ajoute le nom de chaque OWF
  })
)

# Générer le graphique avec facet_wrap
biomass_plot <- ggplot(relative_biomass_combined) +
  geom_line(aes(x = year, y = mean, color = OWF)) +
  geom_ribbon(aes(x = year, ymin = mean - sd, ymax = mean + sd, fill = OWF), alpha = 0.2) +
  geom_hline(yintercept = 1, color = "black", linetype = "dotted") +
  
  # Annotation spécifique aux quatre premiers parcs
  geom_rect(data = subset(relative_biomass_combined, OWF %in% OWF_names[1:4]),
            aes(xmin = 2023, xmax = 2025, ymin = -Inf, ymax = Inf),
            fill = "grey", alpha = 0.01)+
  # Annotation spécifique aux trois derniers parcs
  geom_rect(data = subset(relative_biomass_combined, OWF %in% OWF_names[5:7]),
            aes(xmin = 2028, xmax = 2030, ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.01) +
  geom_rect(data = subset(relative_biomass_combined, OWF %in% OWF_names[8]),
            aes(xmin = 2033, xmax = 2035, ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.01) +
  scale_color_manual(values = colour_palette) +
  scale_fill_manual(values = colour_palette) +
  facet_wrap(~ OWF, scales = "free_y") +
  ylab("biomass ratio") +
  xlim(2010, 2050) +
  # faire attention aux limits ici
  ylim(0.5, 1.8) +
  ggtitle(paste("biomasse totale relative à la référence, scénario équilibre*", regulation)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.position = "none")

print(biomass_plot)

ggsave(file.path("figures/publication/time_series", regulation, "total_biomass_by_OWF.png"), biomass_plot, width = 15, height = 8, dpi = 600)
