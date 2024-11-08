# trophic level heatmap
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
years_cut <- c(2011,2020,2021,2035,2036,2050)
n_years_cut <- c(10,21,22,34,35,49)
n_replicates <- 30
all_species <- c(1:16)
pel_species <- c(10:14)
dem_species <- c(1:9,15:16)
species_group_list <- list(all_species, pel_species, dem_species)
species_group_name <- c("all","pelagic","demersal")

# define scenario for analysis and file paths
regulation <- regulation_scenarios[2]
results_path_base<- file.path("outputs/results_2510","Base_simu","output","CIEM")

# set longitude and latitude
lon <- seq(-1.95,2.45,by=0.1)
lat <- seq(49.05,51.15,by=0.1)

process_maps <- function(species_group,species_group_name,n_years_begin, n_years_end){
  map_base_list <- lapply(1:n_replicates, function(simulation){
    # open nc file
    tl_nc_base <- nc_open(list_tl_nc_base[simulation])
    #read variable
    tl_base <- ncvar_get(tl_nc_base,"TL")
    #close nc file
    nc_close(tl_nc_base)
    
    # repeat same process for biomass (for weighted average of trophic level)
    biomass_nc_base <- nc_open(list_biomass_nc_base[simulation])
    biomass_base <- ncvar_get(biomass_nc_base,"Biomass")
    nc_close(biomass_nc_base)
    
    # select one period
    tl_base <- tl_base[,,,n_years_begin:n_years_end]
    biomass_base <- biomass_base[,,,n_years_begin:n_years_end]
    
    # calculate average trophic level weighted by biomass
    weighted_tl_base <- apply(tl_base * biomass_base, c(1, 2, 4), sum) / apply(biomass_base, c(1, 2, 4), sum)
    
    # calculate average of all years
    tl_average_base <- apply(weighted_tl_base, c(1, 2), mean, na.rm = TRUE)
    
    return(tl_average_base)
  })
  
  map_current_list <- lapply(1:n_replicates, function(simulation){
    # open nc files
    tl_nc_current <- nc_open(list_tl_nc_current[simulation])
    #read variable
    tl_current <- ncvar_get(tl_nc_current,"TL")
    # close nc file
    nc_close(tl_nc_current)
    
    # repeat same process for biomass (for weighted average of trophic level)
    biomass_nc_current <- nc_open(list_biomass_nc_current[simulation])
    biomass_current <- ncvar_get(biomass_nc_current,"Biomass")
    nc_close(biomass_nc_current)
    
    # select one period
    tl_current <- tl_current[,,,n_years_begin:n_years_end]
    biomass_current <- biomass_current[,,,n_years_begin:n_years_end]
    
    # calculate average trophic level weighted by biomass
    weighted_tl_current <- apply(tl_current * biomass_current, c(1, 2, 4), sum) / apply(biomass_current, c(1, 2, 4), sum)
    
    # calculate average of all years
    tl_average_current <- apply(weighted_tl_current, c(1, 2), mean, na.rm = TRUE)
    
    return(tl_average_current)
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
    list_tl_nc_current <- list.files(results_path_scenario, "Yansong_spatializedTL_Simu.", full.names = TRUE)
    list_tl_nc_base <- list.files(results_path_base, "Yansong_spatializedTL_Simu.", full.names = TRUE)
    list_biomass_nc_current <- list.files(results_path_scenario, "Yansong_spatializedBiomass_Simu.", full.names = TRUE)
    list_biomass_nc_base <- list.files(results_path_base, "Yansong_spatializedBiomass_Simu.", full.names = TRUE)
    
    
    # apply the function to three periods and three species groups
    # for(group in 1:length(species_group_list)){
    group <- 1
    tl_table_1 <- process_maps(species_group_list[[group]],species_group_name[group], n_years_cut[1],n_years_cut[2])
    tl_table_2 <- process_maps(species_group_list[[group]],species_group_name[group], n_years_cut[3],n_years_cut[4])
    tl_table_3 <- process_maps(species_group_list[[group]],species_group_name[group], n_years_cut[5],n_years_cut[6])
    # process_maps(species_group_list[[group]],species_group_name[group], n_years_cut[5],n_years_cut[6])
    # }
    # } 
    
    # add label "period"
    tl_table_1$period <- "before OWF construction"
    tl_table_2$period <- "during OWF construction"
    tl_table_3$period <- "after OWF construction"
    # combine three periods
    tl_table <- rbind(tl_table_1,tl_table_2,tl_table_3)
    tl_table$period <- factor(tl_table$period, levels = c("before OWF construction", "during OWF construction", "after OWF construction"))
    
    ratio_map_plot <- ggplot() +
      geom_tile(data = tl_table, aes(x = lon, y = lat, fill = ratio)) +
      scale_fill_gradientn(
        colors = c("darkgrey", "white", "darkred"), 
        values = scales::rescale(c(0.96, 1, 1.04)),   
        limits = c(0.96, 1.04),                        
        oob = scales::squish,
      ) +
      facet_grid(~period)+
      geom_point(data = tl_table[tl_table$OWF,],           
                 aes(x = lon, y = lat), color = "black", size = 1)+
      labs(title = paste0("average trophic level under ",deployment," scenario + ",regulation," scenario"),
           x = "Longitude (°)", y = "Latitude (°)", fill="tl change") +
      theme_bw()+
      theme(plot.title = element_text(size = 14),
            text = element_text(size = 14),
            strip.text = element_text(size = 14, face = "bold"))
    
    print(ratio_map_plot)
    
    ggsave(file.path("figures/publication/heatmap",regulation,deployment,"trophic_level_heatmap.png"),
           ratio_map_plot, width = 16, height = 4, dpi = 600)
  }
}
