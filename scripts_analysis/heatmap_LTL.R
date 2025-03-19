# LTL heatmap
# Auteur : Yansong Huang
# Date de création : 2025-01-22

library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(purrr)
library(ncdf4)

nc_ERSEM <- nc_open("osmose-eec_v4.4_yansong/Base/input/ERSEM_nc/interpolated_CERES_NorthSea_2d_monthly_2002_2022.nc")

LTL_names <- c("depositBenthos","suspensionBenthos","meioBenthos","diatoms",
               "microPhytoplankton","mesoZooplankton","microZooplankton","heterotrophicFlagellates")

diatom <- ncvar_get(nc_ERSEM, varid = "diatoms")
micro_phytoplankton <- ncvar_get(nc_ERSEM, varid = "microPhytoplankton")
meso_zooplankton <- ncvar_get(nc_ERSEM, varid = "mesoZooplankton")
micro_zooplankton <- ncvar_get(nc_ERSEM, varid = "microZooplankton")
heterotrophic_flagellates <- ncvar_get(nc_ERSEM, varid = "heterotrophicFlagellates")
meio_benthos <- ncvar_get(nc_ERSEM, varid = "meioBenthos")
suspension_feeder <- ncvar_get(nc_ERSEM, varid = "suspensionBenthos")
deposit_feeder <- ncvar_get(nc_ERSEM, varid = "depositBenthos")

# calculate mean value of the last year
diatom_subset <- diatom[,,457:480]
micro_phytoplankton_subset <- micro_phytoplankton[,,457:480]
meso_zooplankton_subset <- meso_zooplankton[,,457:480]
micro_zooplankton_subset <- micro_zooplankton[,,457:480]
heterotrophic_flagellates_subset <- heterotrophic_flagellates[,,457:480]
meio_benthos_subset <- meio_benthos[,,457:480]
suspension_feeder_subset <- suspension_feeder[,,457:480]
deposit_feeder_subset <- deposit_feeder[,,457:480]

diatom_mean <- rowMeans(diatom_subset, dims = 2) # dims = 2 means keeping the first two dimensions
micro_phytoplankton_mean <- rowMeans(micro_phytoplankton_subset, dims=2)
meso_zooplankton_mean <- rowMeans(meso_zooplankton_subset, dims=2)
micro_zooplankton_mean <- rowMeans(micro_zooplankton_subset, dims=2)
heterotrophic_flagellates_mean <- rowMeans(heterotrophic_flagellates_subset, dims=2)
meio_benthos_mean <- rowMeans(meio_benthos_subset, dims = 2)
suspension_feeder_mean <- rowMeans(suspension_feeder_subset, dims = 2)
deposit_feeder_mean <- rowMeans(deposit_feeder_subset, dims = 2)

nc_close(nc_ERSEM)
# set longitude and latitude
lon <- seq(-1.95,2.45,by=0.1)
lat <- seq(49.05,51.15,by=0.1)

# Prepare grid and mask
map_grid <- expand.grid(lon = lon, lat = lat)
map_grid$diatoms <- as.vector(diatom_mean)
map_grid$micro_phytoplanktons <- as.vector(micro_phytoplankton_mean)
map_grid$meso_zooplanktons <- as.vector(meso_zooplankton_mean)
map_grid$micro_zooplanktons <- as.vector(micro_zooplankton_mean)
map_grid$heterotrophic_flagellates <- as.vector(heterotrophic_flagellates_mean)
map_grid$meio_benthos <- as.vector(meio_benthos_mean)
map_grid$suspension_feeders <- as.vector(suspension_feeder_mean)
map_grid$deposit_feeders <- as.vector(deposit_feeder_mean)

# Filter to cut off west of Cotentin
map_grid_cut <- filter(map_grid, lon > -1.6)

# transform to long data
# map_grid_long <- gather(map_grid_cut, key = 'LTL_group', value = 'biomass', -c("lon","lat"))

map_diatoms <- ggplot(map_grid_cut[,c(1,2,3)]) +
  geom_tile(data = , aes(x = lon, y = lat, fill = diatoms)) +
  scale_fill_gradient(low = "white", high = "darkgreen")+
  labs(title = "Diatoms biomass",
    x = "Longitude (E)", y = "Latitude (N)", fill = "biomass (t)") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 9),
    text = element_text(size = 9),
    strip.text = element_text(size = 9),
    legend.title = element_text(size = 9),       # 图例标题字体
    legend.text = element_text(size = 8),
  )

ggsave("figures/publication/LTL_heatmap/diatoms_2049.png",
       map_diatoms, width = 4.5, height = 2.5, dpi = 600)
  
map_micro_phytoplankton <- ggplot(map_grid_cut[,c(1,2,4)]) +
  geom_tile(data = , aes(x = lon, y = lat, fill = micro_phytoplanktons)) +
  scale_fill_gradient(low = "white", high = "darkgreen")+
  labs(title = "Micro-phytoplanktons biomass",
       x = "Longitude (E)", y = "Latitude (N)", fill = "biomass (t)") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 9),
    text = element_text(size = 9),
    strip.text = element_text(size = 9),
    legend.title = element_text(size = 9),       # 图例标题字体
    legend.text = element_text(size = 8),
  )

ggsave("figures/publication/LTL_heatmap/micro_phytoplankton_2049.png",
       map_micro_phytoplankton, width = 4.5, height = 2.5, dpi = 600)

map_meso_zooplankton <- ggplot(map_grid_cut[,c(1,2,5)]) +
  geom_tile(data = , aes(x = lon, y = lat, fill = meso_zooplanktons)) +
  scale_fill_gradient(low = "white", high = "darkgreen")+
  labs(title = "Meso-zooplanktons biomass",
       x = "Longitude (E)", y = "Latitude (N)", fill = "biomass (t)") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 9),
    text = element_text(size = 9),
    strip.text = element_text(size = 9),
    legend.title = element_text(size = 9),       # 图例标题字体
    legend.text = element_text(size = 8),
  )

ggsave("figures/publication/LTL_heatmap/meso_zooplankton_2049.png",
       map_meso_zooplankton, width = 4.5, height = 2.5, dpi = 600)

map_micro_zooplankton <- ggplot(map_grid_cut[,c(1,2,6)]) +
  geom_tile(data = , aes(x = lon, y = lat, fill = micro_zooplanktons)) +
  scale_fill_gradient(low = "white", high = "darkgreen")+
  labs(title = "Micro-zooplanktons biomass",
       x = "Longitude (E)", y = "Latitude (N)", fill = "biomass (t)") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 9),
    text = element_text(size = 9),
    strip.text = element_text(size = 9),
    legend.title = element_text(size = 9),       # 图例标题字体
    legend.text = element_text(size = 8),
  )

ggsave("figures/publication/LTL_heatmap/micro_zooplankton_2049.png",
       map_micro_zooplankton, width = 4.5, height = 2.5, dpi = 600)

map_heterotrophic_flagellates <- ggplot(map_grid_cut[,c(1,2,7)]) +
  geom_tile(data = , aes(x = lon, y = lat, fill = heterotrophic_flagellates)) +
  scale_fill_gradient(low = "white", high = "darkgreen")+
  labs(title = "Heterotrophic flagellates biomass",
       x = "Longitude (E)", y = "Latitude (N)", fill = "biomass (t)") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 9),
    text = element_text(size = 9),
    strip.text = element_text(size = 9),
    legend.title = element_text(size = 9),       # 图例标题字体
    legend.text = element_text(size = 8),
  )

ggsave("figures/publication/LTL_heatmap/heterotrophic_flagellates_2049.png",
       map_heterotrophic_flagellates, width = 4.5, height = 2.5, dpi = 600)

map_meio_benthos <- ggplot(map_grid_cut[,c(1,2,8)]) +
  geom_tile(data = , aes(x = lon, y = lat, fill = meio_benthos)) +
  scale_fill_gradient(low = "white", high = "darkgreen")+
  labs(title = "Meio-benthos biomass",
       x = "Longitude (E)", y = "Latitude (N)", fill = "biomass (t)") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 9),
    text = element_text(size = 9),
    strip.text = element_text(size = 9),
    legend.title = element_text(size = 9),       # 图例标题字体
    legend.text = element_text(size = 8),
  )

ggsave("figures/publication/LTL_heatmap/meio_benthos_2049.png",
       map_meio_benthos, width = 4.5, height = 2.5, dpi = 600)

map_suspension_feeders <- ggplot(map_grid_cut[,c(1,2,9)]) +
  geom_tile(data = , aes(x = lon, y = lat, fill = suspension_feeders)) +
  scale_fill_gradient(low = "white", high = "darkgreen")+
  labs(title = "Suspension feeders biomass",
       x = "Longitude (E)", y = "Latitude (N)", fill = "biomass (t)") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 9),
    text = element_text(size = 9),
    strip.text = element_text(size = 9),
    legend.title = element_text(size = 9),       # 图例标题字体
    legend.text = element_text(size = 8),
  )

ggsave("figures/publication/LTL_heatmap/suspension_feeders_2049.png",
       map_suspension_feeders, width = 4.5, height = 2.5, dpi = 600)

map_deposit_feeders <- ggplot(map_grid_cut[,c(1,2,10)]) +
  geom_tile(data = , aes(x = lon, y = lat, fill = deposit_feeders)) +
  scale_fill_gradient(low = "white", high = "darkgreen")+
  labs(title = "Deposit feeders biomass",
       x = "Longitude (E)", y = "Latitude (N)", fill = "biomass (t)") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 9),
    text = element_text(size = 9),
    strip.text = element_text(size = 9),
    legend.title = element_text(size = 9),       # 图例标题字体
    legend.text = element_text(size = 8),
  )

ggsave("figures/publication/LTL_heatmap/deposit_feeders_2049.png",
       map_deposit_feeders, width = 4.5, height = 2.5, dpi = 600)
