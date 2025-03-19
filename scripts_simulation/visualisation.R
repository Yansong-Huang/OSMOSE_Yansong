# scenario visualisation
# Auteur : Yansong Huang
# Date de création : 19/03/2025

library(ncdf4)
library(ggplot2)


fermeture_nc <- nc_open("data_scenarios/protection/sans_fermeture/fishing-distrib.nc")
fermeture_map <- ncvar_get(fermeture_nc, "fishing_area", start = c(1,1,31), count = c(-1,-1,1))
nc_close(fermeture_total_nc)
sum(fermeture_map, na.rm = TRUE) #464


source("scripts_analysis/OWF_mask.R")
# set longitude and latitude
lon <- seq(-1.95,2.45,by=0.1)
lat <- seq(49.05,51.15,by=0.1)

# Prepare grid and mask
map_grid <- expand.grid(lon = lon, lat = lat)
map_grid$effort <- as.vector(fermeture_map)
map_grid$OWF <- as.vector(get("mask_OWF_loin"))
sum(as.vector(get("mask_OWF_cout")))
sum(as.vector(get("mask_OWF_protection")))
sum(as.vector(get("mask_OWF_loin")))
sum(as.vector(get("mask_OWF_equilibre")))


effort_map <- ggplot() +
  geom_tile(data = map_grid, aes(x = lon, y = lat, fill = effort)) +
  scale_fill_gradient2(low = "darkorange", mid = "white", high = "darkgreen") +
  geom_point(data = map_grid[map_grid$OWF,],
             aes(x = lon, y = lat), color = "black", size = 1)+
  labs(title = "fishing effort",
       x = "Longitude (°)", y = "Latitude (°)", fill="effort") +
  theme_bw()+
  theme(plot.title = element_text(size = 14),
        text = element_text(size = 14),
        strip.text = element_text(size = 14, face = "bold"))

print(effort_map)
