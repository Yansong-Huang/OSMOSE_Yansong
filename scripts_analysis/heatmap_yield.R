# yield heat map
# Auteur : Yansong Huang
# Date de création : 2024-10-31

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
    # open nc files
    yield_nc_base <- nc_open(list_yield_nc_base[simulation])
    #read variable
    yield_base <- ncvar_get(yield_nc_base,"YieldBiomass")
    nc_close(yield_nc_base)
    
    # select one period
    yield_base <- yield_base[,,,n_years_begin:n_years_end]
    # sum up all species and all years
    yield_total_base <- apply(yield_base, c(1,2), sum)
    
    return(yield_total_base)
  })
  
  map_current_list <- lapply(1:n_replicates, function(simulation){
    # open nc files
    yield_nc_current <- nc_open(list_yield_nc_current[simulation])
    #read variable
    yield_current <- ncvar_get(yield_nc_current,"YieldBiomass")
    nc_close(yield_nc_current)
    
    # select one period
    yield_current <- yield_current[,,,n_years_begin:n_years_end]
    # sum up all species and all years
    yield_total_current <- apply(yield_current, c(1,2), sum)
    
    return(yield_total_current)
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
    list_yield_nc_current <- list.files(results_path_scenario, "Yansong_spatializedYieldBiomass_Simu.", full.names = TRUE)
    list_yield_nc_base <- list.files(results_path_base, "Yansong_spatializedYieldBiomass_Simu.", full.names = TRUE)
    
    # apply the function to three periods and three species groups
    # for(group in 1:length(species_group_list)){
    group <- 1
    yield_table_1 <- process_maps(species_group_list[[group]],species_group_name[group], n_years_cut[1],n_years_cut[2])
    # yield_table_2 <- process_maps(species_group_list[[group]],species_group_name[group], n_years_cut[3],n_years_cut[4])
    yield_table_3 <- process_maps(species_group_list[[group]],species_group_name[group], n_years_cut[5],n_years_cut[6])
    # process_maps(species_group_list[[group]],species_group_name[group], n_years_cut[5],n_years_cut[6])
    # }
    # } 
    
    # add label "period"
    yield_table_1$period <- "before OWF construction"
    # yield_table_2$period <- "during OWF construction"
    yield_table_3$period <- "after OWF construction"
    # combine three periods
    # yield_table <- rbind(yield_table_1,yield_table_2,yield_table_3)
    yield_table <- rbind(yield_table_1,yield_table_3)
    yield_table$period <- factor(yield_table$period, levels = c("before OWF construction", "during OWF construction", "after OWF construction"))
    
    ratio_map_plot <- ggplot() +
      geom_tile(data = yield_table, aes(x = lon, y = lat, fill = ratio)) +
      scale_fill_gradientn(
        colors = c("darkorange", "white", "darkviolet"),  # 设置颜色梯度：深色-白色-深色
        values = scales::rescale(c(0.75, 1, 1.25)),   # 重新调整颜色的分布
        limits = c(0.75, 1.25),                        # 设置颜色梯度的范围
        oob = scales::squish,
      ) +
      facet_grid(~period)+
      geom_point(data = yield_table[yield_table$OWF,],           
                 aes(x = lon, y = lat), color = "black", size = 1)+
      labs(title = paste0("capture totale scénario ",deployment,"* scénario ",regulation),
           x = "Longitude (°)", y = "Latitude (°)", fill="catch change") +
      theme_bw()+
      theme(plot.title = element_text(size = 14),
            text = element_text(size = 14),
            strip.text = element_text(size = 14, face = "bold"))
    
    print(ratio_map_plot)
    
    ggsave(file.path("figures/publication/heatmap",regulation,deployment,"yield_heatmap_without_construction.png"),
           ratio_map_plot, width = 11, height = 4, dpi = 600)
  }
}
