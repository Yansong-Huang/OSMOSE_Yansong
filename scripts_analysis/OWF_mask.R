library(sf)
# read OWF cells number
OWF_1_cells <- st_read(file.path("OWF_grid",paste("OWF_1.shp",sep = "")))$id
OWF_2_cells <- st_read(file.path("OWF_grid",paste("OWF_2.shp",sep = "")))$id
deployment_scenario_cells <- st_read(file.path("OWF_grid",paste("OWF_equilibre.shp",sep = "")))$id

lon_OWF <- c(OWF_1_cells%%45, OWF_2_cells%%45, deployment_scenario_cells%%45)
lat_OWF <- c(OWF_1_cells%/%45+1, OWF_2_cells%/%45+1, deployment_scenario_cells%/%45+1)

lon_OWF <- c(deployment_scenario_cells%%45)
lat_OWF <- c(deployment_scenario_cells%/%45+1)

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
