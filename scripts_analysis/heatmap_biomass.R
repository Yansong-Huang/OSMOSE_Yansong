# biomass heatmap
# Auteur : Yansong Huang
# Date de création : 2024-08-21

library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(purrr)
library(ncdf4)

# global variables
source("scripts_analysis/OWF_mask.R")
deployment_scenarios <- c("cout","protection","loin","equilibre")
regulation_scenarios <- c("sans_fermeture","fermeture_chalut","fermeture_totale")
CC_scenarios <- c("ON","OFF")
year_begin <- 2002
year_end <- 2050
years_cut <- c(2011,2022,2023,2034,2035,2050)
n_years_cut <- c(10,21,22,33,34,49)
n_replicates <- 30


# define scenario for analysis and file paths
results_path_base<- file.path("outputs/results_2510","Base_simu","output","CIEM")

# set longitude and latitude
lon <- seq(-1.95,2.45,by=0.1)
lat <- seq(49.05,51.15,by=0.1)

process_maps <- function(n_years_begin, n_years_end){
  map_base_list <- lapply(1:n_replicates, function(simulation){
    # open nc files
    biomass_nc_base <- nc_open(list_biomass_nc_base[simulation])
    #read variable
    biomass_base <- ncvar_get(biomass_nc_base,"Biomass")
    nc_close(biomass_nc_base)
    
    # select one period
    biomass_base <- biomass_base[,,,n_years_begin:n_years_end]
    # sum up all species and all years
    biomass_total_base <- apply(biomass_base, c(1,2), sum)

    return(biomass_total_base)
  })
  
  map_current_list <- lapply(1:n_replicates, function(simulation){
    # open nc files
    biomass_nc_current <- nc_open(list_biomass_nc_current[simulation])
    #read variable
    biomass_current <- ncvar_get(biomass_nc_current,"Biomass")
    nc_close(biomass_nc_current)
    
    # select one period
    biomass_current <- biomass_current[,,,n_years_begin:n_years_end]
    # sum up all species and all years
    biomass_total_current <- apply(biomass_current, c(1,2), sum)
    
    return(biomass_total_current)
  })
  # flatten the list to a vector
  flattened_map_base <- unlist(map_base_list)
  flattened_map_current <- unlist(map_current_list)
  
  # build a 3D array
  map_base_array <- array(flattened_map_base, dim = c(45,22,n_replicates))
  map_current_array <- array(flattened_map_current, dim = c(45,22,n_replicates))
  
  # calculate mean for both maps
  mean_map_base <- apply(map_base_array,c(1,2),mean)
  mean_map_current <- apply(map_current_array,c(1,2),mean)
  map_ratio <- mean_map_current/mean_map_base
  
  # expand grid
  map_grid <- expand.grid(lon=lon, lat=lat)
  map_grid$ratio <- as.vector(map_ratio)
  map_grid$OWF <- as.vector(get(paste0("mask_OWF_", deployment)))
  
  # cut off the colomn west of Cotentin
  map_grid_cut <- filter(map_grid, lon>-1.6)
  return(map_grid_cut)
}

for (regulation in regulation_scenarios){
  for (deployment in deployment_scenarios){
    results_path_scenario <- file.path("outputs/results_2510",paste0("CC.",CC_scenarios[1],"_",deployment,"_",regulation),"Base","output","CIEM")
    list_biomass_nc_current <- list.files(results_path_scenario, "Yansong_spatializedBiomass_Simu.", full.names = TRUE)
    list_biomass_nc_base <- list.files(results_path_base, "Yansong_spatializedBiomass_Simu.", full.names = TRUE)
    
    biomass_table_1 <- process_maps(n_years_cut[1],n_years_cut[2])
    biomass_table_2 <- process_maps(n_years_cut[3],n_years_cut[4])
    biomass_table_3 <- process_maps(n_years_cut[5],n_years_cut[6])
    
    # add label "period"
    biomass_table_1$period <- "2011-2022"
    biomass_table_2$period <- "2023-2034"
    biomass_table_3$period <- "2035-2049"
    # combine three periods
    biomass_table <- rbind(biomass_table_1,biomass_table_2,biomass_table_3)
    biomass_table$period <- factor(biomass_table$period, levels = c("2011-2022", "2023-2034", "2035-2049"))
    
    ratio_map_plot <- ggplot() +
      geom_tile(data = biomass_table, aes(x = lon, y = lat, fill = ratio)) +
      scale_fill_gradient2(low = "darkorange", mid = "white", high = "darkgreen", midpoint = 1) +
      facet_grid(~period)+
      geom_point(data = biomass_table[biomass_table$OWF,],           
                 aes(x = lon, y = lat), color = "black", size = 1)+
      labs(title = paste0("total biomass under ",deployment," scenario + ",regulation," scenario"),
           x = "Longitude (°)", y = "Latitude (°)", fill="biomass change") +
      theme_bw()+
      theme(plot.title = element_text(size = 14),
            text = element_text(size = 14),
            strip.text = element_text(size = 14, face = "bold"))

    print(ratio_map_plot)
    
    ggsave(file.path("figures/publication/heatmap",regulation,deployment,"biomass_heatmap.png"),
           ratio_map_plot, width = 16, height = 4, dpi = 600)
  }
}
    