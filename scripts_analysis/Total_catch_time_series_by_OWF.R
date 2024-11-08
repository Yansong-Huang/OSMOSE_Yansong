# Calculs des capture totale au sein de chaque parc éolien
# Auteur : Yansong Huang
# Date de création : 2024-10-30

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
year_end <- 2050
n_years <- 49
# cut_off_year <- 9 # commence la visualisation de l'année 2010
n_replicate <- 30
n_species <- 16

OWF_names <- c("Calvados", "Fecamp", "Dieppe", "Dunkerque", "Centre Manche 1", "Centre Manche 2", "Rampion", "Future OWF")
repeated_OWF_names <- rep(OWF_names, each = 2)
colour_palette <- c(rep("darkred",4),rep("darkgreen",3),rep("darkblue",2))


# chemin pour tous les résultats
regulation <- regulation_scenarios[3]
results_path_base <- file.path("outputs/results_2510","Base_simu","output","CIEM")
results_path_OWF <- file.path("outputs/results_2510",paste0("CC.",CC_scenarios[1],"_",deployment_scenarios[4],"_",regulation),"Base","output","CIEM")

# Chargement les résultats de sortie du modèle
relative_catch_summary <- list()

process_catch <- function(current_results_path) {
  # define nc file list
  list_catch_nc_base <- list.files(results_path_base, "Yansong_spatializedYieldBiomass_Simu.", full.names = TRUE)
  list_catch_nc_current <- list.files(current_results_path, "Yansong_spatializedYieldBiomass_Simu.", full.names = TRUE)
  
  # define years range
  years <- year_begin:year_end
  n_years <- length(years)
  
  # a list for stocking results by OWF of all replicates
  relative_catch_by_OWF_all_replicates <- list()
  
  for (replicate in 1:n_replicate) {
    # a temperal list for save results of each replicate
    relative_catch_by_OWF_temp <- list()
    
    # open nc files
    catch_nc_base <- nc_open(list_catch_nc_base[replicate])
    catch_nc_current <- nc_open(list_catch_nc_current[replicate])
    # read catch from nc files
    catch_spatial_base <- ncvar_get(catch_nc_base, "YieldBiomass")
    catch_spatial_current <- ncvar_get(catch_nc_current, "YieldBiomass")
    nc_close(catch_nc_base)
    nc_close(catch_nc_current)
    
    # loop over each OWF
    for (OWF_name in names(OWF_locations)) {
      lon_index <- OWF_locations[[OWF_name]]$lon
      lat_index <- OWF_locations[[OWF_name]]$lat
      catch_spatial_OWF_base_list <- lapply(1:length(lon_index), function(i) {
        catch_spatial_base[lon_index[i], lat_index[i], , ]
      })
      catch_spatial_OWF_current_list <- lapply(1:length(lon_index), function(i) {
        catch_spatial_current[lon_index[i], lat_index[i], , ]
      })
      
      catch_spatial_OWF_base <- simplify2array(catch_spatial_OWF_base_list)
      catch_spatial_OWF_current <- simplify2array(catch_spatial_OWF_current_list)
      # order des dimensions (n_species, length(lon_index), n_years)
      catch_spatial_OWF_base <- aperm(catch_spatial_OWF_base, perm = c(3, 1, 2))
      catch_spatial_OWF_current <- aperm(catch_spatial_OWF_current, perm = c(3, 1, 2))
      
      # aggregate all cells within the OWF
      total_catch_in_OWF_base <- apply(catch_spatial_OWF_base, 3, sum)
      total_catch_in_OWF_current <- apply(catch_spatial_OWF_current, 3, sum)
      
      # avoid dividing by 0
      total_catch_in_OWF_base[total_catch_in_OWF_base == 0] <- 1e-10
      relative_catch_by_OWF_temp[[OWF_name]] <- total_catch_in_OWF_current / total_catch_in_OWF_base
    }
    
    # stock each replicate to the main list
    for (OWF_name in names(relative_catch_by_OWF_temp)) {
      if (is.null(relative_catch_by_OWF_all_replicates[[OWF_name]])) {
        relative_catch_by_OWF_all_replicates[[OWF_name]] <- matrix(nrow = n_years, ncol = n_replicate)
      }
      relative_catch_by_OWF_all_replicates[[OWF_name]][, replicate] <- relative_catch_by_OWF_temp[[OWF_name]]
    }
  }
  
  # calculate mean and sd
  relative_catch_by_OWF_summary <- list()
  for (OWF_name in names(relative_catch_by_OWF_all_replicates)) {
    yearly_data <- relative_catch_by_OWF_all_replicates[[OWF_name]]
    yearly_mean <- rowMeans(yearly_data)
    yearly_sd <- apply(yearly_data, 1, sd)
    relative_catch_by_OWF_summary[[OWF_name]] <- data.frame(
      year = years,
      mean = yearly_mean,
      sd = yearly_sd
    )
  }
  
  return(relative_catch_by_OWF_summary)
}

# relative_catch_summary <- lapply(scenario_path, process_catch)
relative_catch_summary <- process_catch(results_path_OWF)


# figure composée
relative_catch_combined <- bind_rows(
  lapply(1:length(relative_catch_summary), function(i) {
    relative_catch_summary[[i]] %>%
      mutate(OWF = factor(OWF_names[i], levels = OWF_names))  # Ajoute le nom de chaque OWF
  })
)

# Générer le graphique avec facet_wrap
catch_plot <- ggplot(relative_catch_combined) +
  geom_line(aes(x = year, y = mean, color = OWF)) +
  geom_ribbon(aes(x = year, ymin = mean - sd, ymax = mean + sd, fill = OWF), alpha = 0.2) +
  geom_hline(yintercept = 1, color = "black", linetype = "dotted") +
  
  # Annotation spécifique aux quatre premiers parcs
  geom_rect(data = subset(relative_catch_combined, OWF %in% OWF_names[1:4]),
            aes(xmin = 2023, xmax = 2025, ymin = -Inf, ymax = Inf),
            fill = "grey", alpha = 0.01)+
  # Annotation spécifique aux trois derniers parcs
  geom_rect(data = subset(relative_catch_combined, OWF %in% OWF_names[5:7]),
            aes(xmin = 2028, xmax = 2030, ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.01) +
  geom_rect(data = subset(relative_catch_combined, OWF %in% OWF_names[8]),
            aes(xmin = 2033, xmax = 2035, ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.01) +
  scale_color_manual(values = colour_palette) +
  scale_fill_manual(values = colour_palette) +
  facet_wrap(~ OWF, scales = "free_y") +
  ylab("catch ratio") +
  xlim(2010, 2050) +
  # ylim(0, 2.5) +
  ggtitle(paste("capture totale relative à la référence, scénario équilibre*", regulation)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.position = "none")

print(catch_plot)

ggsave(file.path("figures/publication/time_series", regulation, "total_catch_by_OWF.png"), catch_plot, width = 15, height = 8, dpi = 600)
