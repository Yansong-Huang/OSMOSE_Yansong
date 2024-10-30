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
    biomass_species_base <- map_dfc(1:10, function(simulation) {
      biomass_brut_base <- read.csv(list_biomass_base[simulation], skip = 10)
      biomass_brut_base_species <- biomass_brut_base[,species+1]
      return(biomass_brut_base_species)
    })
    biomass_species_current <- map_dfc(1:10, function(simulation) {
      biomass_brut_current <- read.csv(list_biomass_current[simulation], skip = 10)
      biomass_brut_current_species <- biomass_brut_current[,species+1]
      return(biomass_brut_current_species)
    })
    
    species_list <- c("lesserSpottedDogfish", "redMullet", "pouting", "whiting", "poorCod", "cod", "dragonet", "sole", "plaice", "horseMackerel", "mackerel", "herring", "sardine", "squids", "cuttlefish", "thornbackRay")
    
    return(data.frame(
      replicates = rep(1:10, each = 40),
      period = rep(c(rep("before",12),rep("during",12),rep("after",16)),10),
      species_name = species_list[species],
      ratio = unlist(biomass_species_current)/unlist(biomass_species_base),
    ))
  }))
  
  return(biomass_ratio)
}

# apply to all scenarios
all_biomass <- lapply(results_path, process_biomass)

all_biomass_1 <- as.data.frame(all_biomass[[1]])
all_biomass_1$regulation <- "no_closure"
all_biomass_2 <- as.data.frame(all_biomass[[2]])
all_biomass_2$regulation <- "trawler_closure"
all_biomass_3 <- as.data.frame(all_biomass[[3]])
all_biomass_3$regulation <- "complete_closure"

all_biomass_table <- rbind(all_biomass_1,all_biomass_2,all_biomass_3)

# create plot 
boxplot_biomass <- ggplot(all_biomass_table)+
  geom_boxplot(aes(x = period, y = ratio, fill = period))+
  ylab("biomass ratio")+
  facet_wrap(~species_name)+
  theme_bw()

print(boxplot_biomass)
# Création du graphique de comparaison
# 
# mean_colour_palette <- c("brown",brewer.pal(12, "Paired"))
# sd_colour_palette <- mean_colour_palette[c(1,3,5,7,9,11,13,15)]
# 
# biomass_comparison_plot <- ggplot() +
#   geom_line(data = all_biomass[[1]], aes(x = year, y = biomass_ratio_mean, color = "mean no closure")) +
#   geom_ribbon(data = all_biomass[[1]], aes(x = year,
#                                            ymin = biomass_ratio_mean - biomass_ratio_sd,
#                                            ymax = biomass_ratio_mean + biomass_ratio_sd,
#                                            fill = "sd no closure"),
#               alpha = 0.2) +
#   geom_line(data = all_biomass[[2]], aes(x = year, y = biomass_ratio_mean, color = "mean trawlers closure")) +
#   geom_ribbon(data = all_biomass[[2]], aes(x = year,
#                                            ymin = biomass_ratio_mean - biomass_ratio_sd,
#                                            ymax = biomass_ratio_mean + biomass_ratio_sd,
#                                            fill = "sd trawlers closure"),
#               alpha = 0.2) +
#   geom_line(data = all_biomass[[3]], aes(x = year, y = biomass_ratio_mean, color = "mean complete closure")) +
#   geom_ribbon(data = all_biomass[[3]], aes(x = year,
#                                            ymin = biomass_ratio_mean - biomass_ratio_sd,
#                                            ymax = biomass_ratio_mean + biomass_ratio_sd,
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
#   facet_wrap(~period+species_name, scales = "free_y", ncol = 4) +  # Specify ncol parameter as 4
#   ylab("biomass ratio") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.background = element_rect(fill = "white"),
#         legend.title = element_blank())
# 
# print(biomass_comparison_plot)
# # Sauvegarder le graphique de comparaison
# ggsave(file.path("figures_CIEM_CC_old", deployment, "relative_biomass_all_regulations.png", sep=""), biomass_comparison_plot, width = 15, height = 8, dpi = 600)
# 
