# Explorations & analyses des résultats des simulations
# Auteur : Yansong Huang
# Date de création : 2024-08-14

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
deployment <- deployment_scenarios[4]
results_path_2 <- file.path("results_tl",paste0("CC.",CC_scenarios[1],"_",deployment,"_",regulation_scenarios[1]),"Base","output","CIEM")
results_path_3 <- file.path("results_tl",paste0("CC.",CC_scenarios[1],"_",deployment,"_",regulation_scenarios[2]),"Base","output","CIEM")
results_path_4 <- file.path("results_tl",paste0("CC.",CC_scenarios[1],"_",deployment,"_",regulation_scenarios[3]),"Base","output","CIEM")

results_path <- list(results_path_2, results_path_3, results_path_4)

# Chargement les résultats de sortie du modèle
all_biomass <- list()

###### biomass ######
process_biomass <- function(current_results_path) {
  results_path_base <- file.path("results_tl","Base_simu","output","CIEM")
  list_biomass_base <- list.files(results_path_base, "Yansong_biomass_Simu.", full.names = TRUE)
  list_biomass_current <- list.files(current_results_path, "Yansong_biomass_Simu.", full.names = TRUE)
  
  biomass_ratio <- bind_rows(lapply(c(1:5,7:16), function(species) {
    biomass_species <- map_dfc(1:10, function(simulation) {
      biomass_brut_base <- read.csv(list_biomass_base[simulation], skip = 9)
      biomass_brut_current <- read.csv(list_biomass_current[simulation], skip = 9)
      biomass_ratio <- biomass_brut_current[,species+1] / biomass_brut_base[,species+1]
    })
    
    species_list <- c("lesserSpottedDogfish", "redMullet", "pouting", "whiting", "poorCod", "cod", "dragonet", "sole", "plaice", "horseMackerel", "mackerel", "herring", "sardine", "squids", "cuttlefish", "thornbackRay")
    data.frame(
      year = 2010:2050,
      species_name = species_list[species],
      biomass_ratio_mean = rowMeans(biomass_species),
      biomass_ratio_sd = apply(biomass_species, 1, sd)
    )
  }))
  
  return(biomass_ratio)
}

# apply to all scenarios
all_biomass <- lapply(results_path, process_biomass)

# Création du graphique de comparaison

mean_colour_palette <- c("brown",brewer.pal(12, "Paired"))
sd_colour_palette <- mean_colour_palette[c(1,3,5,7,9,11,13,15)]

biomass_comparison_plot <- ggplot() +
  geom_line(data = all_biomass[[1]], aes(x = year, y = biomass_ratio_mean, color = "mean no closure")) +
  geom_ribbon(data = all_biomass[[1]], aes(x = year,
                                           ymin = biomass_ratio_mean - biomass_ratio_sd,
                                           ymax = biomass_ratio_mean + biomass_ratio_sd,
                                           fill = "sd no closure"),
              alpha = 0.2) +
  geom_line(data = all_biomass[[2]], aes(x = year, y = biomass_ratio_mean, color = "mean trawlers closure")) +
  geom_ribbon(data = all_biomass[[2]], aes(x = year,
                                           ymin = biomass_ratio_mean - biomass_ratio_sd,
                                           ymax = biomass_ratio_mean + biomass_ratio_sd,
                                           fill = "sd trawlers closure"),
              alpha = 0.2) +
  geom_line(data = all_biomass[[3]], aes(x = year, y = biomass_ratio_mean, color = "mean complete closure")) +
  geom_ribbon(data = all_biomass[[3]], aes(x = year,
                                           ymin = biomass_ratio_mean - biomass_ratio_sd,
                                           ymax = biomass_ratio_mean + biomass_ratio_sd,
                                           fill = "sd complete closure"),
              alpha = 0.2) +
  geom_hline(yintercept = 1, color = "black")+
  geom_vline(xintercept = 2023, color = "black", linetype = "dotted")+
  geom_vline(xintercept = 2028, color = "black", linetype = "dotted")+
  geom_vline(xintercept = 2033, color = "black", linetype = "dotted")+
  scale_color_manual(name = element_blank(),
                     values = mean_colour_palette,
                     breaks = c("mean no closure","sd no closure","mean trawlers closure","sd trawlers closure","mean complete closure","sd complete closure"),
                     labels = c("mean no closure","sd no closure","mean trawlers closure","sd trawlers closure","mean complete closure","sd complete closure")) +
  scale_fill_manual(name = element_blank(),
                    values = sd_colour_palette,
                    breaks = c("sd no closure","sd trawlers closure","sd complete closure"),
                    labels = c("sd no closure","sd trawlers closure","sd complete closure")) +
  facet_wrap(~species_name, scales = "free_y", ncol = 4) +  # Specify ncol parameter as 4
  ylab("biomass ratio") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.title = element_blank())

print(biomass_comparison_plot)
# Sauvegarder le graphique de comparaison
ggsave(file.path("figures_CIEM_CC_old", deployment, "relative_biomass_all_regulations.png", sep=""), biomass_comparison_plot, width = 15, height = 8, dpi = 600)

###### yield ######
process_yield <- function(current_results_path) {
  results_path_base <- file.path("results_tl","Base_simu","output","CIEM")
  list_yield_base <- list.files(results_path_base, "Yansong_yield_Simu.", full.names = TRUE)
  list_yield_current <- list.files(current_results_path, "Yansong_yield_Simu.", full.names = TRUE)

  yield_ratio <- bind_rows(lapply(c(1:4,8:16), function(species) {
    yield_species <- map_dfc(1:10, function(simulation) {
      yield_brut_base <- read.csv(list_yield_base[simulation], skip = 9)
      yield_brut_current <- read.csv(list_yield_current[simulation], skip = 9)
      yield_ratio <- yield_brut_current[,species+1] / yield_brut_base[,species+1]
    })

    species_list <- c("lesserSpottedDogfish", "redMullet", "pouting", "whiting", "poorCod", "cod", "dragonet", "sole", "plaice", "horseMackerel", "mackerel", "herring", "sardine", "squids", "cuttlefish", "thornbackRay")
    data.frame(
      year = 2010:2050,
      species_name = species_list[species],
      yield_ratio_mean = rowMeans(yield_species),
      yield_ratio_sd = apply(yield_species, 1, sd)
    )
  }))

  return(yield_ratio)
}

# apply to all scenarios
all_yield <- lapply(results_path, process_yield)

# Création du graphique de comparaison

yield_mean_colour_palette <- c("brown",brewer.pal(12, "Paired"))
yield_sd_colour_palette <- yield_mean_colour_palette[c(1,3,5,7,9,11,13,15)]

yield_comparison_plot <- ggplot() +
  geom_line(data = all_yield[[1]], aes(x = year, y = yield_ratio_mean, color = "mean no closure")) +
  geom_ribbon(data = all_yield[[1]], aes(x = year,
                                           ymin = yield_ratio_mean - yield_ratio_sd,
                                           ymax = yield_ratio_mean + yield_ratio_sd,
                                           fill = "sd no closure"),
              alpha = 0.2) +
  geom_line(data = all_yield[[2]], aes(x = year, y = yield_ratio_mean, color = "mean trawlers closure")) +
  geom_ribbon(data = all_yield[[2]], aes(x = year,
                                           ymin = yield_ratio_mean - yield_ratio_sd,
                                           ymax = yield_ratio_mean + yield_ratio_sd,
                                           fill = "sd trawlers closure"),
              alpha = 0.2) +
  geom_line(data = all_yield[[3]], aes(x = year, y = yield_ratio_mean, color = "mean complete closure")) +
  geom_ribbon(data = all_yield[[3]], aes(x = year,
                                           ymin = yield_ratio_mean - yield_ratio_sd,
                                           ymax = yield_ratio_mean + yield_ratio_sd,
                                           fill = "sd complete closure"),
              alpha = 0.2) +
  geom_hline(yintercept = 1, color = "black")+
  geom_vline(xintercept = 2023, color = "black", linetype = "dotted")+
  geom_vline(xintercept = 2028, color = "black", linetype = "dotted")+
  geom_vline(xintercept = 2033, color = "black", linetype = "dotted")+
  scale_color_manual(name = element_blank(),
                     values = yield_mean_colour_palette,
                     breaks = c("mean no closure","sd no closure","mean trawlers closure","sd trawlers closure","mean complete closure","sd complete closure"),
                     labels = c("mean no closure","sd no closure","mean trawlers closure","sd trawlers closure","mean complete closure","sd complete closure")) +
  scale_fill_manual(name = element_blank(),
                    values = yield_sd_colour_palette,
                    breaks = c("sd no closure","sd trawlers closure","sd complete closure"),
                    labels = c("sd no closure","sd trawlers closure","sd complete closure")) +
  facet_wrap(~species_name, scales = "free_y", ncol = 4) +  # Specify ncol parameter as 4
  ylab("yield ratio") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.title = element_blank())

print(biomass_comparison_plot)
# Sauvegarder le graphique de comparaison
ggsave(file.path("figures_CIEM_CC_old", deployment, "relative_yield_all_regulations_2010.png", sep=""), yield_comparison_plot, width = 15, height = 8, dpi = 600)

###### trophic level ######
process_TL <- function(current_results_path) {
  results_path_base <- file.path("results_tl","Base_simu","output","CIEM","Trophic")
  current_results_path <- file.path(current_results_path,"Trophic")
  list_TL_base <- list.files(results_path_base,"Yansong_meanTL_Simu.", full.names = TRUE)
  list_TL_current <- list.files(current_results_path,"Yansong_meanTL_Simu.", full.names = TRUE)

  TL_ratio <- bind_rows(lapply(1:16, function(species) {
    TL_species <- map_dfc(1:10, function(simulation) {
      TL_brut_base <- read.csv(list_TL_base[simulation], skip = 9)
      TL_brut_current <- read.csv(list_TL_current[simulation], skip = 9)
      TL_ratio <- TL_brut_current[,species+1] / TL_brut_base[,species+1]
    })

    species_list <- c("lesserSpottedDogfish", "redMullet", "pouting", "whiting", "poorCod", "cod", "dragonet", "sole", "plaice", "horseMackerel", "mackerel", "herring", "sardine", "squids", "cuttlefish", "thornbackRay")
    data.frame(
      year = 2010:2050,
      species_name = species_list[species],
      TL_ratio_mean = rowMeans(TL_species),
      TL_ratio_sd = apply(TL_species, 1, sd)
    )
  }))

  return(TL_ratio)
}

# apply to all scenarios
all_TL <- lapply(results_path, process_TL)

# Création du graphique de comparaison

TL_mean_colour_palette <- c("brown",brewer.pal(12, "Paired"))
TL_sd_colour_palette <- TL_mean_colour_palette[c(1,3,5,7,9,11,13,15)]

TL_comparison_plot <- ggplot() +
  geom_line(data = all_TL[[1]], aes(x = year, y = TL_ratio_mean, color = "mean no closure")) +
  geom_ribbon(data = all_TL[[1]], aes(x = year,
                                           ymin = TL_ratio_mean - TL_ratio_sd,
                                           ymax = TL_ratio_mean + TL_ratio_sd,
                                           fill = "sd no closure"),
              alpha = 0.2) +
  geom_line(data = all_TL[[2]], aes(x = year, y = TL_ratio_mean, color = "mean trawlers closure")) +
  geom_ribbon(data = all_TL[[2]], aes(x = year,
                                           ymin = TL_ratio_mean - TL_ratio_sd,
                                           ymax = TL_ratio_mean + TL_ratio_sd,
                                           fill = "sd trawlers closure"),
              alpha = 0.2) +
  geom_line(data = all_TL[[3]], aes(x = year, y = TL_ratio_mean, color = "mean complete closure")) +
  geom_ribbon(data = all_TL[[3]], aes(x = year,
                                           ymin = TL_ratio_mean - TL_ratio_sd,
                                           ymax = TL_ratio_mean + TL_ratio_sd,
                                           fill = "sd complete closure"),
              alpha = 0.2) +
  geom_hline(yintercept = 1, color = "black", linetype = "dotted")+
  scale_color_manual(name = element_blank(),
                     values = TL_mean_colour_palette,
                     breaks = c("mean no closure","sd no closure","mean trawlers closure","sd trawlers closure","mean complete closure","sd complete closure"),
                     labels = c("mean no closure","sd no closure","mean trawlers closure","sd trawlers closure","mean complete closure","sd complete closure")) +
  scale_fill_manual(name = element_blank(),
                    values = TL_sd_colour_palette,
                    breaks = c("sd no closure","sd trawlers closure","sd complete closure"),
                    labels = c("sd no closure","sd trawlers closure","sd complete closure")) +
  geom_hline(yintercept = 1, color = "black")+
  geom_vline(xintercept = 2023, color = "black", linetype = "dotted")+
  geom_vline(xintercept = 2028, color = "black", linetype = "dotted")+
  geom_vline(xintercept = 2033, color = "black", linetype = "dotted")+
  facet_wrap(~species_name, scales = "free_y", ncol = 4) +  # Specify ncol parameter as 4
  ylab("TL ratio") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.title = element_blank())

print(TL_comparison_plot)
# Sauvegarder le graphique de comparaison
ggsave(file.path("figures_CIEM_CC_old", deployment, "relative_TL_all_regulations_2010.png", sep=""), TL_comparison_plot, width = 15, height = 8, dpi = 600)


# ###### yield (absolute value version) ######
# process_yield <- function(current_results_path) {
#   list_yield <- list.files(current_results_path, "Yansong_yield_Simu.", full.names = TRUE)
# 
#   yield_total <- bind_rows(lapply(1:16, function(species) {
#     yield_species <- map_dfc(1:10, function(simulation) {
#       yield_brut <- read.csv(list_yield[simulation], skip = 1)
#       yield_brut <- yield_brut[, species + 1]
#     })
#     species_list <- c("lesserSpottedDogfish", "redMullet", "pouting", "whiting", "poorCod", "cod", "dragonet", "sole", "plaice", "horseMackerel", "mackerel", "herring", "sardine", "squids", "cuttlefish", "thornbackRay")
#     data.frame(
#       year = 2002:2050,
#       species_name = species_list[species],
#       yield_output_mean = rowMeans(yield_species),
#       yield_output_sd = apply(yield_species, 1, sd)
#     )
#   }))
# 
#   return(yield_total)
# }
# 
# # apply to all scenarios
# all_yield <- lapply(results_path, process_yield)
# 
# # Création du graphique de comparaison
# 
# yield_mean_colour_palette <- c("brown",brewer.pal(12, "Paired"))
# yield_sd_colour_palette <- yield_mean_colour_palette[c(1,3,5,7,9,11,13,15)]
# 
# yield_comparison_plot <- ggplot() +
#   geom_line(data = all_yield[[1]], aes(x = year, y = yield_output_mean, color = "mean control")) +
#   geom_ribbon(data = all_yield[[1]], aes(x = year,
#                                          ymin = yield_output_mean - yield_output_sd,
#                                          ymax = yield_output_mean + yield_output_sd,
#                                          fill = "sd control"),
#               alpha = 0.2) +
#   geom_line(data = all_yield[[2]], aes(x = year, y = yield_output_mean, color = "mean no closure")) +
#   geom_ribbon(data = all_yield[[2]], aes(x = year,
#                                          ymin = yield_output_mean - yield_output_sd,
#                                          ymax = yield_output_mean + yield_output_sd,
#                                          fill = "sd no closure"),
#               alpha = 0.2) +
#   geom_line(data = all_yield[[3]], aes(x = year, y = yield_output_mean, color = "mean trawlers closure")) +
#   geom_ribbon(data = all_yield[[3]], aes(x = year,
#                                          ymin = yield_output_mean - yield_output_sd,
#                                          ymax = yield_output_mean + yield_output_sd,
#                                          fill = "sd trawlers closure"),
#               alpha = 0.2) +
#   geom_line(data = all_yield[[4]], aes(x = year, y = yield_output_mean, color = "mean complete closure")) +
#   geom_ribbon(data = all_yield[[4]], aes(x = year,
#                                          ymin = yield_output_mean - yield_output_sd,
#                                          ymax = yield_output_mean + yield_output_sd,
#                                          fill = "sd complete closure"),
#               alpha = 0.2) +
#   scale_color_manual(name = element_blank(),
#                      values = yield_mean_colour_palette,
#                      breaks = c("observed data","mean control","sd control","mean no closure","sd no closure","mean trawlers closure","sd trawlers closure","mean complete closure","sd complete closure"),
#                      labels = c("observed data","mean control","sd control","mean no closure","sd no closure","mean trawlers closure","sd trawlers closure","mean complete closure","sd complete closure")) +
#   scale_fill_manual(name = element_blank(),
#                     values = yield_sd_colour_palette,
#                     breaks = c("sd control","sd no closure","sd trawlers closure","sd complete closure"),
#                     labels = c("sd control","sd no closure","sd trawlers closure","sd complete closure")) +
#   facet_wrap(~species_name, scales = "free_y", ncol = 4) +  # Specify ncol parameter as 4
#   ylab("yield (t)") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.background = element_rect(fill = "white"),
#         legend.title = element_blank())
# 
# print(yield_comparison_plot)
# # Sauvegarder le graphique de comparaison
# ggsave(file.path("figures_CIEM",deployment, "yield_all_regulations.png", sep=""), yield_comparison_plot, width = 15, height = 8, dpi = 600)
