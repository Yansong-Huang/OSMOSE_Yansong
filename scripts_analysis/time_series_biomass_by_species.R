# Séries temporelles de biomasse par espèce
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
# variables globales
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
regulation <- regulation_scenarios[3]
results_path_1 <- file.path("outputs/results_2510","Base_simu","output","CIEM")
results_path_2 <- file.path("outputs/results_2510",paste0("CC.",CC_scenarios[1],"_",deployment_scenarios[1],"_",regulation),"Base","output","CIEM")
results_path_3 <- file.path("outputs/results_2510",paste0("CC.",CC_scenarios[1],"_",deployment_scenarios[2],"_",regulation),"Base","output","CIEM")
results_path_4 <- file.path("outputs/results_2510",paste0("CC.",CC_scenarios[1],"_",deployment_scenarios[3],"_",regulation),"Base","output","CIEM")
results_path_5 <- file.path("outputs/results_2510",paste0("CC.",CC_scenarios[1],"_",deployment_scenarios[4],"_",regulation),"Base","output","CIEM")

scenario_path <- list(results_path_2, results_path_3, results_path_4, results_path_5)

# Chargement les résultats de sortie du modèle
all_biomass <- list()

process_biomass <- function(current_results_path) {
  list_biomass_scenario <- list.files(current_results_path, "Yansong_biomass_Simu.", full.names = TRUE)
  list_biomass_reference <- list.files(results_path_1, "Yansong_biomass_Simu.", full.names = TRUE)
  
  biomass_summary <- bind_rows(lapply(c(1:5,7:n_species), function(species) {
    biomass_species <- map_dfc(1:n_replicate, function(simulation) {
      biomass_brut_scenario <- read.csv(list_biomass_scenario[simulation], skip = 1)
      biomass_brut_simulation <- read.csv(list_biomass_reference[simulation], skip = 1)
      biomass_ratio <- biomass_brut_scenario / biomass_brut_simulation
      biomass_ratio_species <- biomass_ratio[cut_off_year:n_years, species + 1]
    })
    species_list <- c("lesserSpottedDogfish", "redMullet", "pouting", "whiting", "poorCod","cod", "dragonet", "sole", "plaice", "horseMackerel", "mackerel", "herring", "sardine", "squids", "cuttlefish", "thornbackRay")
    data.frame(
      year = 2010:2050,
      species_name = species_list[species],
      biomass_output_mean = rowMeans(biomass_species),
      biomass_output_sd = apply(biomass_species, 1, sd)
    )
  }))
  
  return(biomass_summary)
}

# apply to all scenarios
all_biomass <- lapply(scenario_path, process_biomass)

# time series of biomass by species

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
  ggtitle(paste("biomasse par espèces relative à la simulation référente, scénario",regulation))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.title = element_blank())

print(biomass_plot)
# Sauvegarder le graphique de comparaison
ggsave(file.path("figures/publication/time_series",regulation, "biomass_by_species.png", sep=""), biomass_plot, width = 15, height = 8, dpi = 600)


