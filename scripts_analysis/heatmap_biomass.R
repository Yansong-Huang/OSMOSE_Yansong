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
library(egg)

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
    biomass_nc_base <- nc_open(list_biomass_nc_base[simulation])
    biomass_base <- ncvar_get(biomass_nc_base, "Biomass")
    nc_close(biomass_nc_base)
    
    # select period and sum over species and years
    biomass_base <- biomass_base[,,,n_years_begin:n_years_end]
    apply(biomass_base, c(1, 2), sum)
  })
  
  map_current_list <- lapply(1:n_replicates, function(simulation){
    biomass_nc_current <- nc_open(list_biomass_nc_current[simulation])
    biomass_current <- ncvar_get(biomass_nc_current, "Biomass")
    nc_close(biomass_nc_current)
    
    # select period and sum over species and years
    biomass_current <- biomass_current[,,,n_years_begin:n_years_end]
    apply(biomass_current, c(1, 2), sum)
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
  map_grid$OWF <- as.vector(get(paste0("mask_OWF_", deployment_scenarios[4])))
  
  # Filter to cut off west of Cotentin
  map_grid_cut <- filter(map_grid, lon > -1.6)
  
  return(map_grid_cut)
}

biomass_table_all <- data.frame()

for (regulation in regulation_scenarios){
    results_path_scenario <- file.path("outputs/results_1111",paste0("CC.ON_",deployment_scenarios[4],"_",regulation),"Base","output","CIEM")
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
    biomass_table$regulation <- case_when(
      regulation == "sans_fermeture" ~ "no closure during operational phase",
      regulation == "fermeture_chalut" ~ "trawlers closure during operational phase",
      regulation == "fermeture_totale" ~ "complete closure during operational phase",
      TRUE ~ NA_character_
    )
    
    biomass_table_all <- rbind(
      biomass_table_all,
      biomass_table
    )
}

biomass_table_all$regulation <- factor(biomass_table_all$regulation,
                                     levels = c("no closure during operational phase",
                                                "trawlers closure during operational phase",
                                                "complete closure during operational phase"))
biomass_table_all <- biomass_table_all %>%
  filter(regulation %in% c("no closure during operational phase","complete closure during operational phase")) %>%
  filter(period %in% c("2023-2034","2035-2049"))

ratio_map_plot <- ggplot() +
  # 绘制背景热力图
  geom_tile(data = biomass_table_all, aes(x = lon, y = lat, fill = ratio-1)) +
  scale_fill_gradient2(low = "darkorange", mid = "white", high = "darkgreen", midpoint = 0) +
  
  # 为 OWF 点添加图例
  geom_point(data = biomass_table_all[biomass_table_all$OWF & biomass_table_all$period != "2011-2022",],
             aes(x = lon, y = lat, color = "OWF"), size = 1) +
  scale_color_manual(name = "", values = c("OWF" = "black"),guide = guide_legend(order = 1)) +
  
  # 设置分面
  facet_grid(period ~ regulation, scales = "free_y", labeller = labeller(
    period = label_wrap_gen(20), regulation = label_wrap_gen(25))) +
  
  # 添加图例和主题
  labs(title = "Total Biomass",
       x = "Longitude (E)", y = "Latitude (N)", fill = "Biomass change") +
  theme_bw() +
  theme(
    plot.title = element_blank(),
    text = element_text(size = 14),
    strip.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12),       # 图例标题字体
    legend.text = element_text(size = 12),
  )

tagged_facet <- tag_facet(ratio_map_plot, 
                          open = "(", close = ")", tag_pool = letters, 
                          x = Inf, y = -Inf, 
                          hjust = 2.5, vjust = -1, 
                          fontface = "plain")

final_heatmap <- tagged_facet + theme(strip.text = element_text())


 ggsave("figures/publication/heatmap/biomass_heatmap_balance_diapo.png",
        final_heatmap, width = 12, height = 6, dpi = 600)
    