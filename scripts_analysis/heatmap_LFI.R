# LFI heatmap
# Auteur : Yansong Huang
# Date de création : 2024-11-08

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
year_begin <- 2002
year_end <- 2050
years_cut <- c(2011,2022,2023,2034,2035,2050)
n_years_cut <- c(10,21,22,33,34,49)
n_replicates <- 30
# set longitude and latitude
lon <- seq(-1.95,2.45,by=0.1)
lat <- seq(49.05,51.15,by=0.1)
# define reference results path
results_path_base<- file.path("outputs/results_1111","Base_simu","Base","output","CIEM")

process_maps <- function(n_years_begin, n_years_end){
  map_base_list <- lapply(1:n_replicates, function(simulation){
    # open and read nc files
    LFI_nc_base <- nc_open(list_LFI_nc_base[simulation])
    LFI_base <- ncvar_get(LFI_nc_base, "Biomass")
    nc_close(LFI_nc_base)
    
    # select period and sum over species and years
    LFI_base <- LFI_base[,,,n_years_begin:n_years_end]
    apply(LFI_base, c(1, 2), sum)
  })
  
  map_current_list <- lapply(1:n_replicates, function(simulation){
    LFI_nc_current <- nc_open(list_LFI_nc_current[simulation])
    LFI_current <- ncvar_get(LFI_nc_current, "Biomass")
    nc_close(LFI_nc_current)
    
    # select period and sum over species and years
    LFI_current <- LFI_current[,,,n_years_begin:n_years_end]
    apply(LFI_current, c(1, 2), sum)
  })
  
  # Convert lists to 3D arrays
  map_base_array <- array(unlist(map_base_list), dim = c(45, 22, n_replicates))
  map_current_array <- array(unlist(map_current_list), dim = c(45, 22, n_replicates))
  
  # Initialize p-value and ratio matrices
  map_p_value <- matrix(NA, nrow = 45, ncol = 22)
  map_ratio <- matrix(NA, nrow = 45, ncol = 22)
  
  # Vectorized t-test and ratio calculation
  for (lon_temp in 1:45) {
    for (lat_temp in 1:22) {
      if (!is.na(map_base_array[lon_temp, lat_temp, 1])) {
        base_values <- map_base_array[lon_temp, lat_temp, ]
        current_values <- map_current_array[lon_temp, lat_temp, ]
        
        # Vectorized t-test and ratio
        t_result <- try(t.test(base_values, current_values), silent = TRUE)
        map_p_value[lon_temp, lat_temp] <- if (inherits(t_result, "htest")) t_result$p.value else NA
        map_ratio[lon_temp, lat_temp] <- mean(current_values) / mean(base_values)
      }
    }
  }
  
  # Mask out non-significant changes
  map_ratio[map_p_value > 0.05] <- 1
  
  # Prepare grid and mask
  map_grid <- expand.grid(lon = lon, lat = lat)
  map_grid$ratio <- as.vector(map_ratio)
  map_grid$OWF <- as.vector(get(paste0("mask_OWF_", deployment)))
  
  # Filter to cut off west of Cotentin
  map_grid_cut <- filter(map_grid, lon > -1.6)
  
  return(map_grid_cut)
}


for (regulation in regulation_scenarios){
  for (deployment in deployment_scenarios){
    results_path_scenario <- file.path("outputs/results_2510",paste0("CC.ON_",deployment,"_",regulation),"Base","output","CIEM","SizeIndicators")
    list_LFI_nc_current <- list.files(results_path_scenario, "Yansong_biomassDistribBySize_Simu.", full.names = TRUE)
    list_LFI_nc_base <- list.files(results_path_base, "Yansong_biomassDistribBySize_Simu.", full.names = TRUE)
    
    LFI_table_1 <- process_maps(n_years_cut[1],n_years_cut[2])
    LFI_table_2 <- process_maps(n_years_cut[3],n_years_cut[4])
    LFI_table_3 <- process_maps(n_years_cut[5],n_years_cut[6])
    
    # add label "period"
    LFI_table_1$period <- "2011-2022"
    LFI_table_2$period <- "2023-2034"
    LFI_table_3$period <- "2035-2049"
    # combine three periods
    LFI_table <- rbind(LFI_table_1,LFI_table_2,LFI_table_3)
    LFI_table$period <- factor(LFI_table$period, levels = c("2011-2022", "2023-2034", "2035-2049"))
    
    ratio_map_plot <- ggplot() +
      geom_tile(data = LFI_table, aes(x = lon, y = lat, fill = ratio)) +
      scale_fill_gradient2(low = "darkorange", mid = "white", high = "darkgreen", midpoint = 1) +
      facet_grid(~period)+
      geom_point(data = LFI_table[LFI_table$OWF,],           
                 aes(x = lon, y = lat), color = "black", size = 1)+
      labs(title = paste0("total LFI ",deployment," * ",regulation),
           x = "Longitude (°)", y = "Latitude (°)", fill="LFI change") +
      theme_bw()+
      theme(plot.title = element_text(size = 14),
            text = element_text(size = 14),
            strip.text = element_text(size = 14, face = "bold"))
    
    print(ratio_map_plot)
    
    ggsave(file.path("figures/publication/heatmap",regulation,deployment,"LFI_heatmap_significant.png"),
           ratio_map_plot, width = 16, height = 4, dpi = 600)
  }
}
