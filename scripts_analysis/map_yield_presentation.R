# yield map
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
deployment <- deployment_scenarios[4]
CC_scenarios <- c("ON","OFF")
year_begin <- 2002
year_end <- 2050
years_cut <- c(2011,2020,2021,2035,2036,2050)
n_years_cut <- c(10,21,22,34,35,49)
n_replicates <- 10
all_species <- c(1:16)
pel_species <- c(10:14)
dem_species <- c(1:9,15:16)
species_group_list <- list(all_species, pel_species, dem_species)
species_group_name <- c("all","pelagic","demersal")

results_path_1 <- file.path("output/results_tl",paste0("CC.",CC_scenarios[1],"_",deployment,"_",regulation_scenarios[1]),"Base","output","CIEM")
results_path_2 <- file.path("output/results_tl",paste0("CC.",CC_scenarios[1],"_",deployment,"_",regulation_scenarios[2]),"Base","output","CIEM")
results_path_3 <- file.path("output/results_tl",paste0("CC.",CC_scenarios[1],"_",deployment,"_",regulation_scenarios[3]),"Base","output","CIEM")
results_path <- c(results_path_1,results_path_2,results_path_3)
results_path_base<- file.path("output/results_tl","Base_simu","output","CIEM")

# set longitude and latitude
lon <- seq(-1.95,2.45,by=0.1)
lat <- seq(49.05,51.15,by=0.1)

process_maps <- function(species_group,species_group_name,n_years_begin, n_years_end){
  map_base_list <- lapply(1:10, function(simulation){
    # open nc files
    yield_nc_base <- nc_open(list_yield_nc_base[simulation])
    #read variable
    yield_tl_base <- ncvar_get(yield_nc_base,"YieldBiomass")
    nc_close(yield_nc_base)
    
    # cut from one period
    yield_tl_base <- yield_tl_base[,,species_group,n_years_begin:n_years_end] # to be replaced
    
    # sum up all species
    yield_total_base <- apply(yield_tl_base, c(1,2), sum)
    
    return(yield_total_base)
  })
  
  map_current_list <- lapply(1:10, function(simulation){
    # open nc files
    yield_nc_current <- nc_open(list_yield_nc_current[simulation])
    #read variable
    yield_tl_current <- ncvar_get(yield_nc_current,"YieldBiomass")
    nc_close(yield_nc_current)
    
    # cut from one period
    yield_tl_current <- yield_tl_current[,,species_group,n_years_begin:n_years_end]
    
    # sum up all species
    yield_total_current <- apply(yield_tl_current, c(1,2), sum)
    
    return(yield_total_current)
  })
  # flatten the list to a vector
  flattened_map_base <- unlist(map_base_list)
  flattened_map_current <- unlist(map_current_list)
  
  # build a 3D array
  map_base_array <- array(flattened_map_base, dim = c(45,22,10))
  map_current_array <- array(flattened_map_current, dim = c(45,22,10))
  
  # calculate mean for both maps
  mean_map_base <- apply(map_base_array,c(1,2),mean)
  mean_map_current <- apply(map_current_array,c(1,2),mean)
  map_ratio <- mean_map_current/mean_map_base
  
  # expand grid
  map_grid <- expand.grid(lon=lon, lat=lat)
  map_grid$ratio <- as.vector(map_ratio)
  map_grid$OWF <- as.vector(mask_OWF_equilibre)
  
  map_grid_alt <- filter(map_grid, lon>-1.6)
  return(map_grid_alt)
}

# for (reg in 1:length(regulation_scenarios)){
reg <- 2
current_results_path <- results_path[reg]
regulation <- regulation_scenarios[reg]

list_yield_nc_current <- list.files(current_results_path, "Yansong_spatializedYieldBiomass_Simu.", full.names = TRUE)
list_yield_nc_base <- list.files(results_path_base, "Yansong_spatializedYieldBiomass_Simu.", full.names = TRUE)

# apply the function to three periods and three species groups
# for(group in 1:length(species_group_list)){
group <- 1
yield_table_1 <- process_maps(species_group_list[[group]],species_group_name[group], n_years_cut[1],n_years_cut[2])
yield_table_2 <- process_maps(species_group_list[[group]],species_group_name[group], n_years_cut[3],n_years_cut[4])
# process_maps(species_group_list[[group]],species_group_name[group], n_years_cut[5],n_years_cut[6])
# }
# } 

yield_table_1$period <- "before OWF construction"
yield_table_2$period <- "during and after OWF construction"
yield_table <- rbind(yield_table_1,yield_table_2)
ratio_map_plot <- ggplot() +
  geom_tile(data = yield_table, aes(x = lon, y = lat, fill = ratio)) +
  # scale_fill_gradient2(low = "darkorange", mid = "white", high = "darkblue", midpoint = 1, limits = c(0, 1.28)) +
  scale_fill_gradientn(
    colors = c("darkorange", "white", "darkblue"),  # 设置颜色梯度：深色-白色-深色
    values = scales::rescale(c(0.75, 1, 1.25)),   # 重新调整颜色的分布
    limits = c(0.75, 1.25),                        # 设置颜色梯度的范围
    oob = scales::squish,
    ) +
  facet_grid(~period)+
  geom_point(data = yield_table[yield_table$OWF,],           
             aes(x = lon, y = lat), color = "black", size = 1)+
  labs(title = "total yield (balance deployment scenario + trawlers closure scenario)",
       x = "Longitude (°)", y = "Latitude (°)", fill="yield change") +
  theme_bw()+
  theme(plot.title = element_text(size = 14),
        text = element_text(size = 14),
        strip.text = element_text(size = 14, face = "bold"))
# theme(  plot.title = element_text(size = 13),
#         axis.title.x = element_text(size = 13),
#         axis.title.y = element_text(size = 13),
#         axis.text.x = element_text(size = 13),
#         axis.text.y = element_text(size = 13),
#         legend.title = element_text(size = 13),
#         legend.text = element_text(size = 13))
print(ratio_map_plot)


# ggsave(file.path("figures_CIEM_CC_old", "presentation_yield_map.png"),
#        ratio_map_plot, width = 13, height = 5, dpi = 600)
