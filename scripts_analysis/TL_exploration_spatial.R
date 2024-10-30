# Explorations & analyses des résultats spatialisés des simulations
# Auteur : Yansong Huang
# Date de création : 2024-08-19

library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(purrr)
library(ncdf4)
library(sf)

# variable globales
deployment_scenarios <- c("cout","protection","loin","equilibre")
regulation_scenarios <- c("sans_fermeture","fermeture_chalut","fermeture_saisonniere")
CC_scenarios <- c("ON","OFF")
year_begin <- 2002
year_begin_proj <- 2022
year_end <- 2050
n_years <- 49
n_years_proj <- 29
n_replicate <- 10

# define results paths
regulation <- regulation_scenarios[3]
deployment <- deployment_scenarios[4]
results_path_1 <- file.path("results_TL","Base_simu","output","CIEM")
results_path_2 <- file.path("results_TL",paste0("CC.",CC_scenarios[1],"_",deployment,"_",regulation),"Base","output","CIEM")

# read TL nc file
TL_nc <- nc_open(file.path(results_path_1,"Yansong_spatializedTL_Simu0.nc"))
lon <- ncvar_get(TL_nc, "longitude")
lat <- ncvar_get(TL_nc, "latitude")
TL_global <- ncvar_get(TL_nc,"TL")
nc_close(TL_nc)

# read OWF cells number
deployment_scenario_cells <- st_read(file.path("OWF_grid",paste("OWF_",deployment,".shp",sep = "")))$id
lon_OWF <- c(deployment_scenario_cells%%45)
lat_OWF <- c(deployment_scenario_cells%/%45 + 1)

# lon-lat combinations of OWF grids
OWF_combinations <- matrix(c(lat_OWF,lon_OWF), ncol = 2)

# create a mask of 45*22
mask_OWF <- array(FALSE, dim = c(45, 22))

# select OWF cells in the mask 
for (i in 1:nrow(OWF_combinations)) {
  lat <- OWF_combinations[i, 1]
  lon <- OWF_combinations[i, 2]
  mask_OWF[lon, lat] <- TRUE
}

# loop over species
for (species_index in 1:16){
  TL_species <- TL_global[,,species_index,] # 4th dimension-time
  
  # list for two zones
  OWF_TL_list <- list()
  other_TL_list <- list()
  
  index_OWF <- 1
  index_other <- 1
  
  for (longitude in 1:45){
    for (latitude in 1:22){
      if(is.na(TL_species[longitude, latitude,1]) || TL_species[longitude, latitude,1]<1){
      }else{
        if (mask_OWF[longitude,latitude]){
          OWF_TL_list[[index_OWF]] <- TL_species[longitude, latitude, ]
          index_OWF <- index_OWF + 1
        } else{
          other_TL_list[[index_other]] <- TL_species[longitude, latitude, ]
          index_other <- index_other + 1
        }}
    }
  }
  
  # transform in other forms
  OWF_TL_matrix <- do.call(cbind, OWF_TL_list)
  other_TL_matrix <- do.call(cbind, other_TL_list)
  
  # calculate means in all cells
  OWF_TL_sum <- rowMeans(OWF_TL_matrix)
  other_TL_sum <- rowMeans(other_TL_matrix)

  # combine TL in both zones, keep data from 2010 to 2050
  reserve_effect <- data.frame(year=c(2010:2050),
                               OWF_TL <- OWF_TL_sum[9:49],
                               other_TL <- other_TL_sum[9:49])
  
  # standardisation
  reserve_effect$OWF_TL<- reserve_effect$OWF_TL/reserve_effect$OWF_TL[1]
  reserve_effect$other_TL <- reserve_effect$other_TL/reserve_effect$other_TL[1]
  
  # species name for the plot
  species_list <- c("lesserSpottedDogfish", "redMullet", "pouting", "whiting", "poorCod", "cod", "dragonet", "sole", "plaice", "horseMackerel", "mackerel", "herring", "sardine", "squids", "cuttlefish", "thornbackRay")
  
  # create plot
  reserve_effect_plot <- ggplot(reserve_effect)+
    geom_line(aes(x=year, y=OWF_TL), colour="darkred")+
    geom_line(aes(x=year, y=other_TL), colour="darkblue")+
    ggtitle(species_list[species_index])
  
  print(reserve_effect_plot)
  ggsave(file.path("figures_CIEM_CC_old", deployment,regulation, "TL_spatial",paste0(species_list[species_index],"_TL_spatial.png"), sep=""), reserve_effect_plot, width = 15, height = 8, dpi = 600)
  
}
