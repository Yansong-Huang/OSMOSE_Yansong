# Explorations & analyses des résultats des simulations
# Auteur : Yansong Huang
# Date de création : 2024-08-13

library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(purrr)
library(ncdf4)

# variable globales
deployment_scenarios <- c("cout","protection","loin","equilibre")
regulation_scenarios <- c("sans_fermeture","fermeture_chalut","fermeture_totale")
CC_scenarios <- c("ON","OFF")
year_begin <- 2002
year_begin_proj <- 2022
year_end <- 2050
n_years <- 49
n_years_proj <- 29
n_replicate <- 10

# chemin pour tous les résultats
regulation <- regulation_scenarios[1]
results_path_1 <- file.path("outputs/results_tl","Base_simu","output","CIEM")
results_path_2 <- file.path("outputs/results_tl",paste0("CC.",CC_scenarios[1],"_",deployment_scenarios[1],"_",regulation),"Base","output","CIEM")
results_path_3 <- file.path("outputs/results_tl",paste0("CC.",CC_scenarios[1],"_",deployment_scenarios[2],"_",regulation),"Base","output","CIEM")
results_path_4 <- file.path("outputs/results_tl",paste0("CC.",CC_scenarios[1],"_",deployment_scenarios[3],"_",regulation),"Base","output","CIEM")
results_path_5 <- file.path("outputs/results_tl",paste0("CC.",CC_scenarios[1],"_",deployment_scenarios[4],"_",regulation),"Base","output","CIEM")

results_path <- list(results_path_1, results_path_2, results_path_3, results_path_4, results_path_5)

# Chargement les résultats de sortie du modèle
all_biomass <- list()

###### biomass ######
process_biomass <- function(current_results_path) {
  list_biomass <- list.files(current_results_path, "Yansong_biomass_Simu.", full.names = TRUE)

  biomass_total <- bind_rows(lapply(1:16, function(species) {
    biomass_species <- map_dfc(1:10, function(simulation) {
      biomass_brut <- read.csv(list_biomass[simulation], skip = 1)
      biomass_brut <- biomass_brut[, species + 1]
    })
    species_list <- c("lesserSpottedDogfish", "redMullet", "pouting", "whiting", "poorCod", "cod", "dragonet", "sole", "plaice", "horseMackerel", "mackerel", "herring", "sardine", "squids", "cuttlefish", "thornbackRay")
    data.frame(
      year = 2002:2050,
      species_name = species_list[species],
      biomass_output_mean = rowMeans(biomass_species),
      biomass_output_sd = apply(biomass_species, 1, sd)
    )
  }))

  return(biomass_total)
}

# apply to all scenarios
all_biomass <- lapply(results_path, process_biomass)

# Création du graphique de comparaison

biomass_mean_colour_palette <- c("brown",brewer.pal(12, "Paired"))
biomass_sd_colour_palette <- biomass_mean_colour_palette[c(3,5,7,9,11,13,15)]

biomass_comparison_plot <- ggplot() +
  geom_line(data = all_biomass[[1]], aes(x = year, y = biomass_output_mean, color = "mean control")) +
  geom_ribbon(data = all_biomass[[1]], aes(x = year,
                                           ymin = biomass_output_mean - biomass_output_sd,
                                           ymax = biomass_output_mean + biomass_output_sd,
                                           fill = "sd control"),
              alpha = 0.2) +
  geom_line(data = all_biomass[[2]], aes(x = year, y = biomass_output_mean, color = "mean cost")) +
  geom_ribbon(data = all_biomass[[2]], aes(x = year,
                                           ymin = biomass_output_mean - biomass_output_sd,
                                           ymax = biomass_output_mean + biomass_output_sd,
                                           fill = "sd cost"),
              alpha = 0.2) +
  geom_line(data = all_biomass[[3]], aes(x = year, y = biomass_output_mean, color = "mean protection")) +
  geom_ribbon(data = all_biomass[[3]], aes(x = year,
                                           ymin = biomass_output_mean - biomass_output_sd,
                                           ymax = biomass_output_mean + biomass_output_sd,
                                           fill = "sd protection"),
              alpha = 0.2) +
  geom_line(data = all_biomass[[4]], aes(x = year, y = biomass_output_mean, color = "mean distance")) +
  geom_ribbon(data = all_biomass[[4]], aes(x = year,
                                           ymin = biomass_output_mean - biomass_output_sd,
                                           ymax = biomass_output_mean + biomass_output_sd,
                                           fill = "sd distance"),
              alpha = 0.2) +
  geom_line(data = all_biomass[[5]], aes(x = year, y = biomass_output_mean, color = "mean balance")) +
  geom_ribbon(data = all_biomass[[5]], aes(x = year,
                                           ymin = biomass_output_mean - biomass_output_sd,
                                           ymax = biomass_output_mean + biomass_output_sd,
                                           fill = "sd balance"),
              alpha = 0.2) +
  scale_color_manual(name = element_blank(),
                     values = biomass_mean_colour_palette,
                     breaks = c("mean control","sd control","mean cost","sd cost","mean protection","sd protection","mean distance","sd distance", "mean balance", "sd balance"),
                     labels = c("mean control","sd control","mean cost","sd cost","mean protection","sd protection","mean distance","sd distance", "mean balance", "sd balance")) +
  scale_fill_manual(name = element_blank(),
                    values = biomass_sd_colour_palette,
                    breaks = c("sd control","sd cost","sd protection","sd distance","sd balance"),
                    labels = c("sd control","sd cost","sd protection","sd distance","sd balance")) +
  facet_wrap(~species_name, scales = "free_y", ncol = 4) +  # Specify ncol parameter as 4
  ylab("biomass (t)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.title = element_blank())

print(biomass_comparison_plot)
# Sauvegarder le graphique de comparaison
ggsave(file.path("figures/publication",regulation, "biomass_all_deployments.png", sep=""), biomass_comparison_plot, width = 15, height = 8, dpi = 600)

###### yield ######
process_yield <- function(current_results_path) {
  list_yield <- list.files(current_results_path, "Yansong_yield_Simu.", full.names = TRUE)

  yield_total <- bind_rows(lapply(1:16, function(species) {
    yield_species <- map_dfc(1:10, function(simulation) {
      yield_brut <- read.csv(list_yield[simulation], skip = 1)
      yield_brut <- yield_brut[, species + 1]
    })
    species_list <- c("lesserSpottedDogfish", "redMullet", "pouting", "whiting", "poorCod", "cod", "dragonet", "sole", "plaice", "horseMackerel", "mackerel", "herring", "sardine", "squids", "cuttlefish", "thornbackRay")
    data.frame(
      year = 2002:2050,
      species_name = species_list[species],
      yield_output_mean = rowMeans(yield_species),
      yield_output_sd = apply(yield_species, 1, sd)
    )
  }))

  return(yield_total)
}

# apply to all scenarios
all_yield <- lapply(results_path, process_yield)

# Création du graphique de comparaison

yield_mean_colour_palette <- c("brown",brewer.pal(12, "Paired"))
yield_sd_colour_palette <- yield_mean_colour_palette[c(3,5,7,9,11,13,15)]

yield_comparison_plot <- ggplot() +
  geom_line(data = all_yield[[1]], aes(x = year, y = yield_output_mean, color = "mean control")) +
  geom_ribbon(data = all_yield[[1]], aes(x = year,
                                         ymin = yield_output_mean - yield_output_sd,
                                         ymax = yield_output_mean + yield_output_sd,
                                         fill = "sd control"),
              alpha = 0.2) +
  geom_line(data = all_yield[[2]], aes(x = year, y = yield_output_mean, color = "mean cost")) +
  geom_ribbon(data = all_yield[[2]], aes(x = year,
                                         ymin = yield_output_mean - yield_output_sd,
                                         ymax = yield_output_mean + yield_output_sd,
                                         fill = "sd cost"),
              alpha = 0.2) +
  geom_line(data = all_yield[[3]], aes(x = year, y = yield_output_mean, color = "mean protection")) +
  geom_ribbon(data = all_yield[[3]], aes(x = year,
                                         ymin = yield_output_mean - yield_output_sd,
                                         ymax = yield_output_mean + yield_output_sd,
                                         fill = "sd protection"),
              alpha = 0.2) +
  geom_line(data = all_yield[[4]], aes(x = year, y = yield_output_mean, color = "mean distance")) +
  geom_ribbon(data = all_yield[[4]], aes(x = year,
                                         ymin = yield_output_mean - yield_output_sd,
                                         ymax = yield_output_mean + yield_output_sd,
                                         fill = "sd distance"),
              alpha = 0.2) +
  geom_line(data = all_yield[[5]], aes(x = year, y = yield_output_mean, color = "mean balance")) +
  geom_ribbon(data = all_yield[[5]], aes(x = year,
                                         ymin = yield_output_mean - yield_output_sd,
                                         ymax = yield_output_mean + yield_output_sd,
                                         fill = "sd balance"),
              alpha = 0.2) +
  scale_color_manual(name = element_blank(),
                     values = yield_mean_colour_palette,
                     breaks = c("mean control","sd control","mean cost","sd cost","mean protection","sd protection","mean distance","sd distance", "mean balance", "sd balance"),
                     labels = c("mean control","sd control","mean cost","sd cost","mean protection","sd protection","mean distance","sd distance", "mean balance", "sd balance")) +
  scale_fill_manual(name = element_blank(),
                    values = yield_sd_colour_palette,
                    breaks = c("sd control","sd cost","sd protection","sd distance","sd balance"),
                    labels = c("sd control","sd cost","sd protection","sd distance","sd balance")) +
  facet_wrap(~species_name, scales = "free_y", ncol = 4) +  # Specify ncol parameter as 4
  ylab("yield (t)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.title = element_blank())

print(yield_comparison_plot)
# Sauvegarder le graphique de comparaison
ggsave(file.path("figures_CIEM_CC_old", regulation, "yield_all_deployments.png", sep=""), yield_comparison_plot, width = 15, height = 8, dpi = 600)


# ###### yield by fleets ######
# 
# # function for processing catch by fleet
# process_species_yield_fleet <- function(species_idx,list_yield_nc, fleet) {
#   species_list <- c("lesserSpottedDogfish", "redMullet", "pouting", "whiting", "poorCod", "cod", "dragonet", "sole", "plaice", "horseMackerel", "mackerel", "herring", "sardine", "squids", "cuttlefish", "thornbackRay")
#   # Get species name
#   species_name <- species_list[species_idx]
#   
#   # Create a dataframe to store species' years and simulation outputs
#   yield_species <- data.frame(year = year_begin:year_end, species = species_name, stringsAsFactors = FALSE)
#   
#   # Loop through each simulation result file
#   for (simulation in seq_len(n_replicate)) {
#     # Open nc file and get the catch by fleet
#     catch_by_fleet <- nc_open(list_yield_nc[simulation]) %>%
#       ncvar_get(varid = "landings")
#     
#     catch_this_fleet <- catch_by_fleet[fleet,,] %>%
#       t() %>%
#       as.matrix() %>%
#       { array(., c(24, n_years, ncol(.))) } %>% # transfrom from 2D to 3D, 24 is time step and ncol(.) is number of species
#       apply(c(2, 3), sum)
#     
#     # Add each simulation's result to the dataframe
#     yield_species <- yield_species %>%
#       mutate(!!paste0("yield_sim_", simulation) := catch_this_fleet[, species_idx])
#   }
#   
#   # calculate mean and sd for each species
#   yield_species <- yield_species %>%
#     mutate(yield_output_mean = rowMeans(select(., -c(year, species))),
#            yield_output_sd = apply(select(., -c(year, species)), 1, sd))
#   
#   # select colomns for output
#   yield_species <- yield_species %>%
#     select(year, species, yield_output_mean, yield_output_sd)
#   
#   return(yield_species)
# }
# 
# for (deployment in deployment_scenarios){
#   for (regulation in regulation_scenarios){
#     results_path_fleet <- file.path("results",paste0("CC.",CC_scenarios[1],"_",deployment,"_",regulation),"Base","output","CIEM")
#     # Get list of simulation output files
#     list_yield_nc <- list.files(results_path_fleet, "Yansong_yieldByFishery_Simu*", full.names = TRUE)
#     
#     # Process simulation outputs for all species
#     yield_bottom_trawlers <- bind_rows(lapply(1:16, function(species_idx) {
#       process_species_yield_fleet(species_idx, list_yield_nc = list_yield_nc, fleet = 1)
#     }))
#     yield_midwater_trawlers <- bind_rows(lapply(1:16, function(species_idx) {
#       process_species_yield_fleet(species_idx, list_yield_nc = list_yield_nc, fleet = 2)
#     }))
#     yield_netters <- bind_rows(lapply(1:16, function(species_idx) {
#       process_species_yield_fleet(species_idx, list_yield_nc = list_yield_nc, fleet = 3)
#     }))
#     yield_others <- bind_rows(lapply(1:16, function(species_idx) {
#       process_species_yield_fleet(species_idx, list_yield_nc = list_yield_nc, fleet = 4)
#     }))
#     
#     yield_bottom_trawlers_plot <- create_yield_plot(yield_bottom_trawlers, "Bottom Trawlers Catch")
#     yield_midwater_trawlers_plot <- create_yield_plot(yield_midwater_trawlers, "Midwater Trawlers Catch")
#     yield_netters_plot <- create_yield_plot(yield_netters, "Netters Catch")
#     yield_others_plot <- create_yield_plot(yield_others, "Others Catch")
#     
#     print(yield_bottom_trawlers_plot)
#     print(yield_midwater_trawlers_plot)
#     print(yield_netters_plot)
#     print(yield_others_plot)
#     
#     ggsave(file.path("figures_CIEM", deployment, regulation, "yield_bottom_trawlers.png"), yield_bottom_trawlers_plot, width = 10, height = 5, dpi = 600)
#     ggsave(file.path("figures_CIEM", deployment, regulation, "yield_midwater_trawlers.png"), yield_midwater_trawlers_plot, width = 10, height = 5, dpi = 600)
#     ggsave(file.path("figures_CIEM", deployment, regulation, "yield_netters.png"), yield_netters_plot, width = 10, height = 5, dpi = 600)
#     ggsave(file.path("figures_CIEM", deployment, regulation, "yield_others.png"), yield_others_plot, width = 10, height = 5, dpi = 600)
#     
#   }
# }
#   
# 
# # # function for plotting
# # create_yield_plot <- function(data, title) {
# #   ggplot(data = data) +
# #     geom_line(aes(x = year, y = yield_output_mean, color = "darkblue")) +
# #     geom_ribbon(aes(x = year,
# #                     ymin = pmax(yield_output_mean - yield_output_sd, 0),
# #                     ymax = yield_output_mean + yield_output_sd,
# #                     fill = "blue"),
# #                 alpha = 0.2) +
# #     scale_color_manual(name = element_blank(),
# #                        values = c("darkblue" = "darkblue"),
# #                        breaks = c("darkblue"),
# #                        labels = c("mean model outputs")) +
# #     scale_fill_manual(name = element_blank(),
# #                       values = c("blue" = "blue"),
# #                       breaks = c("blue"),
# #                       labels = c("sd model outputs")) +
# #     facet_wrap(~species, scales = "free_y") +
# #     ylab("catch (t)") +
# #     ggtitle(title) +
# #     theme_bw() +
# #     theme(axis.text.x = element_text(angle = 45, hjust = 1),
# #           plot.background = element_rect(fill = "white"),
# #           legend.margin = margin(0, 1, 0, 1),
# #           legend.title = element_blank(),
# #           legend.position = c(0.7, 0.04))
# # }
# # 
# # 
