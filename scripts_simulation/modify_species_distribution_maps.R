# créer cartes de distribution
# date de création : 05/08/2024
###### read shapefiles ######
library(sf)
library(ncdf4)

##### functions ######
# function of Antione. Notice that the index grid_to_close should be coherent between the shapefile and R.
grid_modify <- function(species_grid, grid_owf_scenario, multiplier){
  grid_to_modify <- grid_owf_scenario
  species_grid_modified <- species_grid
  species_grid_modified[grid_to_modify] <- species_grid_modified[grid_to_modify] * multiplier
  return(species_grid_modified)
}
# funtion for modifying distribution maps with multipliers 
apply_multiplier <- function(base_array, multiplier) {
  result <- apply(base_array, MARGIN = 3, function(slice) {
    slice * multiplier
  })
  return(array(result, dim = c(45, 22, 24)))
}

deployment_scenarios <- c("cout","protection","loin","equilibre")


multiplier_base_nc <- nc_open("osmose-eec_v4.4_yansong/Base/input/species_distribution/thornbackRay.nc")
multiplier_base <- ncvar_get(multiplier_base_nc, "stage0", start = c(1,1,1), count = c(-1,-1,1))
# read attributed zones
OWF_1_cells <- st_read(file.path("OWF_grid",paste("OWF_1.shp",sep = "")))$id
OWF_2_cells <- st_read(file.path("OWF_grid",paste("OWF_2.shp",sep = "")))$id
OWF_1_tampon_cells <- st_read(file.path("OWF_grid",paste("tampon_1.shp",sep = "")))$id
OWF_2_tampon_cells <- st_read(file.path("OWF_grid",paste("tampon_2.shp",sep = "")))$id

# loop over SER scenarios
for (deployment in deployment_scenarios){
  # create species distribution folder
  folder_distribution <- file.path("data_scenarios",deployment, "species_distribution")
  if (!dir.exists(folder_distribution)){
    dir.create(folder_distribution, recursive = TRUE)
  }
  
  # read the deployment scenario
  deployment_scenario_cells <- st_read(file.path("OWF_grid",paste("OWF_",deployment,".shp",sep = "")))$id
  deployment_scenario_tampon_cells <- st_read(file.path("OWF_grid",paste("tampon_",deployment,".shp",sep = "")))$id
  
  multiplier_OWF_1 <- grid_modify(multiplier_base, OWF_1_cells, 0.5)
  multiplier_OWF_2 <- grid_modify(multiplier_base, OWF_2_cells, 0.5)
  multiplier_OWF_3 <- grid_modify(multiplier_base, deployment_scenario_cells, 0.5)
  
  multiplier_tampon_1 <- grid_modify(multiplier_base, OWF_1_tampon_cells, 0.5)
  multiplier_tampon_2 <- grid_modify(multiplier_base, OWF_2_tampon_cells, 0.5)
  multiplier_tampon_3 <- grid_modify(multiplier_base, deployment_scenario_tampon_cells, 0.5)
  
  ###### squids ######
  distribution_base_nc <- nc_open("osmose-eec_v4.4_yansong/Base/input/species_distribution/squids.nc")
  distribution_base <- ncvar_get(distribution_base_nc, "stage0")
  nc_close(distribution_base_nc)
  
  # apply the multipliers
  distribution_OWF_1 <- apply_multiplier(distribution_base, multiplier_OWF_1)
  distribution_OWF_2 <- apply_multiplier(distribution_base, multiplier_OWF_2)
  distribution_OWF_3 <- apply_multiplier(distribution_base, multiplier_OWF_3)
  
  # create new nc file
  # define the name of nc file
  nc_name <- file.path(folder_distribution, "squids.nc")
  # define the dimensions in nc files 
  dimLon <- ncdim_def( "longitude", "degree", seq(-1.95,2.45,0.1))
  dimLat <- ncdim_def( "latitude", "degree", seq(49.05,51.15,0.1))
  dimTime <- ncdim_def("time","year",seq(1,49*24,1), unlim=TRUE)
  
  # define the variable according to number of life stage
  stage0 <- ncvar_def("stage0","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9)
  
  # create nc file
  nc_output <- nc_create(nc_name, stage0)
  
  for (i in 1:49) { # entire simulation period
    ncvar_put(nc_output, stage0, distribution_base, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 22:23 ) { # 2023 to 2024
    ncvar_put(nc_output, stage0, distribution_OWF_1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 27:28 ) { # 2028 to 2029
    ncvar_put(nc_output, stage0, distribution_OWF_2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 32:33 ) { # 2033 to 2034
    ncvar_put(nc_output, stage0, distribution_OWF_3, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  nc_close(nc_output)
  
  
  ###### thornback ray ######
  distribution_base_nc <- nc_open("osmose-eec_v4.4_yansong/Base/input/species_distribution/thornbackRay.nc")
  distribution_base <- ncvar_get(distribution_base_nc, "stage0")
  nc_close(distribution_base_nc)
  
  # apply the multipliers
  distribution_OWF_1 <- apply_multiplier(distribution_base, multiplier_OWF_1)
  distribution_OWF_2 <- apply_multiplier(distribution_base, multiplier_OWF_2)
  distribution_OWF_3 <- apply_multiplier(distribution_base, multiplier_OWF_3)
  
  # create new nc file
  # define the name of nc file
  nc_name <- file.path(folder_distribution, "thornbackRay.nc")
  # define the dimensions in nc files 
  dimLon <- ncdim_def( "longitude", "degree", seq(-1.95,2.45,0.1))
  dimLat <- ncdim_def( "latitude", "degree", seq(49.05,51.15,0.1))
  dimTime <- ncdim_def("time","year",seq(1,49*24,1), unlim=TRUE)
  
  # define the variable according to number of life stage
  stage0 <- ncvar_def("stage0","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9)
  
  # create nc file
  nc_output <- nc_create(nc_name, stage0)
  
  for (i in 1:49) { # entire simulation period
    ncvar_put(nc_output, stage0, distribution_base, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 22:23 ) { # 2023 to 2024
    ncvar_put(nc_output, stage0, distribution_OWF_1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 27:28 ) { # 2028 to 2029
    ncvar_put(nc_output, stage0, distribution_OWF_2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 32:33 ) { # 2033 to 2034
    ncvar_put(nc_output, stage0, distribution_OWF_3, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  nc_close(nc_output)
  
  ###### lesser spotted dogfish ######
  distribution_base_nc <- nc_open("osmose-eec_v4.4_yansong/Base/input/species_distribution/lesserSpottedDogfish.nc")
  distribution_base_stage0 <- ncvar_get(distribution_base_nc, "stage0")
  distribution_base_stage1 <- ncvar_get(distribution_base_nc, "stage1")
  nc_close(distribution_base_nc)
  
  # apply the multipliers
  distribution_OWF_1_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_OWF_1)
  distribution_OWF_2_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_OWF_2)
  distribution_OWF_3_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_OWF_3)
  
  distribution_OWF_1_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_OWF_1)
  distribution_OWF_2_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_OWF_2)
  distribution_OWF_3_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_OWF_3)
  
  # create new nc file
  # define the name of nc file
  nc_name <- file.path(folder_distribution, "lesserSpottedDogfish.nc")
  # define the dimensions in nc files 
  dimLon <- ncdim_def( "longitude", "degree", seq(-1.95,2.45,0.1))
  dimLat <- ncdim_def( "latitude", "degree", seq(49.05,51.15,0.1))
  dimTime <- ncdim_def("time","year",seq(1,49*24,1), unlim=TRUE)
  
  # define the variable according to number of life stage
  stage0 <- ncvar_def("stage0","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9, longname = "0-2 years")
  stage1 <- ncvar_def("stage1","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9, longname = "2-11 years")
  
  # create nc file
  nc_output <- nc_create(nc_name, list(stage0, stage1))
  
  for (i in 1:49) { # entire simulation period
    ncvar_put(nc_output, stage0, distribution_base_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_base_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 22:23 ) { # 2023 to 2024
    ncvar_put(nc_output, stage0, distribution_OWF_1_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_1_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 27:28 ) { # 2028 to 2029
    ncvar_put(nc_output, stage0, distribution_OWF_2_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_2_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 32:33 ) { # 2033 to 2034
    ncvar_put(nc_output, stage0, distribution_OWF_3_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_3_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  
  nc_close(nc_output)
  
  
  ###### red mullet ######
  distribution_base_nc <- nc_open("osmose-eec_v4.4_yansong/Base/input/species_distribution/redMullet.nc")
  distribution_base_stage0 <- ncvar_get(distribution_base_nc, "stage0")
  distribution_base_stage1 <- ncvar_get(distribution_base_nc, "stage1")
  nc_close(distribution_base_nc)
  
  # apply the multipliers
  distribution_OWF_1_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_tampon_1)
  distribution_OWF_2_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_tampon_2)
  distribution_OWF_3_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_tampon_3)
  
  distribution_OWF_1_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_tampon_1)
  distribution_OWF_2_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_tampon_2)
  distribution_OWF_3_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_tampon_3)
  
  # create new nc file
  # define the name of nc file
  nc_name <- file.path(folder_distribution, "redMullet.nc")
  # define the dimensions in nc files 
  dimLon <- ncdim_def( "longitude", "degree", seq(-1.95,2.45,0.1))
  dimLat <- ncdim_def( "latitude", "degree", seq(49.05,51.15,0.1))
  dimTime <- ncdim_def("time","year",seq(1,49*24,1), unlim=TRUE)
  
  # define the variable according to number of life stage
  stage0 <- ncvar_def("stage0","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9, longname = "0-1 year")
  stage1 <- ncvar_def("stage1","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9, longname = "1-12 years")
  
  # create nc file
  nc_output <- nc_create(nc_name, list(stage0, stage1))
  
  for (i in 1:49) { # entire simulation period
    ncvar_put(nc_output, stage0, distribution_base_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_base_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 22:23 ) { # 2023 to 2024
    ncvar_put(nc_output, stage0, distribution_OWF_1_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_1_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 27:28 ) { # 2028 to 2029
    ncvar_put(nc_output, stage0, distribution_OWF_2_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_2_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 32:33 ) { # 2033 to 2034
    ncvar_put(nc_output, stage0, distribution_OWF_3_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_3_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  
  nc_close(nc_output)
  
  ###### pouting ######
  distribution_base_nc <- nc_open("osmose-eec_v4.4_yansong/Base/input/species_distribution/pouting.nc")
  distribution_base_stage0 <- ncvar_get(distribution_base_nc, "stage0")
  distribution_base_stage1 <- ncvar_get(distribution_base_nc, "stage1")
  distribution_base_stage2 <- ncvar_get(distribution_base_nc, "stage2")
  nc_close(distribution_base_nc)
  
  # apply the multipliers
  distribution_OWF_1_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_tampon_1)
  distribution_OWF_2_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_tampon_2)
  distribution_OWF_3_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_tampon_3)
  
  distribution_OWF_1_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_tampon_1)
  distribution_OWF_2_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_tampon_2)
  distribution_OWF_3_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_tampon_3)
  
  distribution_OWF_1_stage2 <- apply_multiplier(distribution_base_stage2, multiplier_tampon_1)
  distribution_OWF_2_stage2 <- apply_multiplier(distribution_base_stage2, multiplier_tampon_2)
  distribution_OWF_3_stage2 <- apply_multiplier(distribution_base_stage2, multiplier_tampon_3)
  
  # create new nc file
  # define the name of nc file
  nc_name <- file.path(folder_distribution, "pouting.nc")
  # define the dimensions in nc files 
  dimLon <- ncdim_def( "longitude", "degree", seq(-1.95,2.45,0.1))
  dimLat <- ncdim_def( "latitude", "degree", seq(49.05,51.15,0.1))
  dimTime <- ncdim_def("time","year",seq(1,49*24,1), unlim=TRUE)
  
  # define the variable according to number of life stage
  stage0 <- ncvar_def("stage0","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9)
  stage1 <- ncvar_def("stage1","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9)
  stage2 <- ncvar_def("stage2","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9)
  
  # create nc file
  nc_output <- nc_create(nc_name, list(stage0, stage1, stage2))
  
  for (i in 1:49) { # entire simulation period
    ncvar_put(nc_output, stage0, distribution_base_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_base_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage2, distribution_base_stage2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 22:23 ) { # 2023 to 2024
    ncvar_put(nc_output, stage0, distribution_OWF_1_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_1_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage2, distribution_OWF_1_stage2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 27:28 ) { # 2028 to 2029
    ncvar_put(nc_output, stage0, distribution_OWF_2_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_2_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage2, distribution_OWF_2_stage2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 32:33 ) { # 2033 to 2034
    ncvar_put(nc_output, stage0, distribution_OWF_3_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_3_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage2, distribution_OWF_3_stage2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  
  nc_close(nc_output)
  
  ###### whiting ######
  distribution_base_nc <- nc_open("osmose-eec_v4.4_yansong/Base/input/species_distribution/whiting.nc")
  distribution_base_stage0 <- ncvar_get(distribution_base_nc, "stage0")
  distribution_base_stage1 <- ncvar_get(distribution_base_nc, "stage1")
  distribution_base_stage2 <- ncvar_get(distribution_base_nc, "stage2")
  nc_close(distribution_base_nc)
  
  # apply the multipliers
  distribution_OWF_1_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_tampon_1)
  distribution_OWF_2_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_tampon_2)
  distribution_OWF_3_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_tampon_3)
  
  distribution_OWF_1_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_tampon_1)
  distribution_OWF_2_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_tampon_2)
  distribution_OWF_3_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_tampon_3)
  
  distribution_OWF_1_stage2 <- apply_multiplier(distribution_base_stage2, multiplier_tampon_1)
  distribution_OWF_2_stage2 <- apply_multiplier(distribution_base_stage2, multiplier_tampon_2)
  distribution_OWF_3_stage2 <- apply_multiplier(distribution_base_stage2, multiplier_tampon_3)
  
  # create new nc file
  # define the name of nc file
  nc_name <- file.path(folder_distribution, "whiting.nc")
  # define the dimensions in nc files 
  dimLon <- ncdim_def( "longitude", "degree", seq(-1.95,2.45,0.1))
  dimLat <- ncdim_def( "latitude", "degree", seq(49.05,51.15,0.1))
  dimTime <- ncdim_def("time","year",seq(1,49*24,1), unlim=TRUE)
  
  # define the variable according to number of life stage
  stage0 <- ncvar_def("stage0","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9)
  stage1 <- ncvar_def("stage1","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9)
  stage2 <- ncvar_def("stage2","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9)
  
  # create nc file
  nc_output <- nc_create(nc_name, list(stage0, stage1, stage2))
  
  for (i in 1:49) { # entire simulation period
    ncvar_put(nc_output, stage0, distribution_base_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_base_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage2, distribution_base_stage2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 22:23 ) { # 2023 to 2024
    ncvar_put(nc_output, stage0, distribution_OWF_1_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_1_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage2, distribution_OWF_1_stage2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 27:28 ) { # 2028 to 2029
    ncvar_put(nc_output, stage0, distribution_OWF_2_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_2_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage2, distribution_OWF_2_stage2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 32:33 ) { # 2033 to 2034
    ncvar_put(nc_output, stage0, distribution_OWF_3_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_3_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage2, distribution_OWF_3_stage2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  
  nc_close(nc_output)
  
  ###### cod ######
  distribution_base_nc <- nc_open("osmose-eec_v4.4_yansong/Base/input/species_distribution/cod.nc")
  distribution_base_stage0 <- ncvar_get(distribution_base_nc, "stage0")
  distribution_base_stage1 <- ncvar_get(distribution_base_nc, "stage1")
  distribution_base_stage2 <- ncvar_get(distribution_base_nc, "stage2")
  nc_close(distribution_base_nc)
  
  # apply the multipliers
  distribution_OWF_1_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_tampon_1)
  distribution_OWF_2_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_tampon_2)
  distribution_OWF_3_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_tampon_3)
  
  distribution_OWF_1_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_tampon_1)
  distribution_OWF_2_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_tampon_2)
  distribution_OWF_3_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_tampon_3)
  
  distribution_OWF_1_stage2 <- apply_multiplier(distribution_base_stage2, multiplier_tampon_1)
  distribution_OWF_2_stage2 <- apply_multiplier(distribution_base_stage2, multiplier_tampon_2)
  distribution_OWF_3_stage2 <- apply_multiplier(distribution_base_stage2, multiplier_tampon_3)
  
  # create new nc file
  # define the name of nc file
  nc_name <- file.path(folder_distribution, "cod.nc")
  # define the dimensions in nc files 
  dimLon <- ncdim_def( "longitude", "degree", seq(-1.95,2.45,0.1))
  dimLat <- ncdim_def( "latitude", "degree", seq(49.05,51.15,0.1))
  dimTime <- ncdim_def("time","year",seq(1,49*24,1), unlim=TRUE)
  
  # define the variable according to number of life stage
  stage0 <- ncvar_def("stage0","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9)
  stage1 <- ncvar_def("stage1","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9)
  stage2 <- ncvar_def("stage2","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9)
  
  # create nc file
  nc_output <- nc_create(nc_name, list(stage0, stage1, stage2))
  
  for (i in 1:49) { # entire simulation period
    ncvar_put(nc_output, stage0, distribution_base_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_base_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage2, distribution_base_stage2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 22:23 ) { # 2023 to 2024
    ncvar_put(nc_output, stage0, distribution_OWF_1_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_1_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage2, distribution_OWF_1_stage2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 27:28 ) { # 2028 to 2029
    ncvar_put(nc_output, stage0, distribution_OWF_2_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_2_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage2, distribution_OWF_2_stage2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 32:33 ) { # 2033 to 2034
    ncvar_put(nc_output, stage0, distribution_OWF_3_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_3_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage2, distribution_OWF_3_stage2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  
  nc_close(nc_output)
  
  ###### poorCod ######
  distribution_base_nc <- nc_open("osmose-eec_v4.4_yansong/Base/input/species_distribution/poorCod.nc")
  distribution_base_stage0 <- ncvar_get(distribution_base_nc, "stage0")
  distribution_base_stage1 <- ncvar_get(distribution_base_nc, "stage1")
  nc_close(distribution_base_nc)
  
  # apply the multipliers
  distribution_OWF_1_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_tampon_1)
  distribution_OWF_2_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_tampon_2)
  distribution_OWF_3_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_tampon_3)
  
  distribution_OWF_1_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_tampon_1)
  distribution_OWF_2_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_tampon_2)
  distribution_OWF_3_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_tampon_3)
  
  # create new nc file
  # define the name of nc file
  nc_name <- file.path(folder_distribution, "poorCod.nc")
  # define the dimensions in nc files 
  dimLon <- ncdim_def( "longitude", "degree", seq(-1.95,2.45,0.1))
  dimLat <- ncdim_def( "latitude", "degree", seq(49.05,51.15,0.1))
  dimTime <- ncdim_def("time","year",seq(1,49*24,1), unlim=TRUE)
  
  # define the variable according to number of life stage
  stage0 <- ncvar_def("stage0","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9)
  stage1 <- ncvar_def("stage1","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9)
  
  # create nc file
  nc_output <- nc_create(nc_name, list(stage0, stage1))
  
  for (i in 1:49) { # entire simulation period
    ncvar_put(nc_output, stage0, distribution_base_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_base_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 22:23 ) { # 2023 to 2024
    ncvar_put(nc_output, stage0, distribution_OWF_1_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_1_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 27:28 ) { # 2028 to 2029
    ncvar_put(nc_output, stage0, distribution_OWF_2_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_2_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 32:33 ) { # 2033 to 2034
    ncvar_put(nc_output, stage0, distribution_OWF_3_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_3_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  
  nc_close(nc_output)
  
  
  ###### dragonet ######
  distribution_base_nc <- nc_open("osmose-eec_v4.4_yansong/Base/input/species_distribution/dragonet.nc")
  distribution_base <- ncvar_get(distribution_base_nc, "stage0")
  nc_close(distribution_base_nc)
  
  # apply the multipliers
  distribution_OWF_1 <- apply_multiplier(distribution_base, multiplier_tampon_1)
  distribution_OWF_2 <- apply_multiplier(distribution_base, multiplier_tampon_2)
  distribution_OWF_3 <- apply_multiplier(distribution_base, multiplier_tampon_3)
  
  # create new nc file
  # define the name of nc file
  nc_name <- file.path(folder_distribution, "dragonet.nc")
  # define the dimensions in nc files 
  dimLon <- ncdim_def( "longitude", "degree", seq(-1.95,2.45,0.1))
  dimLat <- ncdim_def( "latitude", "degree", seq(49.05,51.15,0.1))
  dimTime <- ncdim_def("time","year",seq(1,49*24,1), unlim=TRUE)
  
  # define the variable according to number of life stage
  stage0 <- ncvar_def("stage0","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9)
  
  # create nc file
  nc_output <- nc_create(nc_name, stage0)
  
  for (i in 1:49) { # entire simulation period
    ncvar_put(nc_output, stage0, distribution_base, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 22:23 ) { # 2023 to 2024
    ncvar_put(nc_output, stage0, distribution_OWF_1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 27:28 ) { # 2028 to 2029
    ncvar_put(nc_output, stage0, distribution_OWF_2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 32:33 ) { # 2033 to 2034
    ncvar_put(nc_output, stage0, distribution_OWF_3, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  
  nc_close(nc_output)
  
  
  ###### horseMackerel ######
  distribution_base_nc <- nc_open("osmose-eec_v4.4_yansong/Base/input/species_distribution/horseMackerel.nc")
  distribution_base_stage0 <- ncvar_get(distribution_base_nc, "stage0")
  distribution_base_stage1 <- ncvar_get(distribution_base_nc, "stage1")
  distribution_base_stage2 <- ncvar_get(distribution_base_nc, "stage2")
  nc_close(distribution_base_nc)
  
  # apply the multipliers
  distribution_OWF_1_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_tampon_1)
  distribution_OWF_2_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_tampon_2)
  distribution_OWF_3_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_tampon_3)
  
  distribution_OWF_1_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_tampon_1)
  distribution_OWF_2_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_tampon_2)
  distribution_OWF_3_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_tampon_3)
  
  distribution_OWF_1_stage2 <- apply_multiplier(distribution_base_stage2, multiplier_tampon_1)
  distribution_OWF_2_stage2 <- apply_multiplier(distribution_base_stage2, multiplier_tampon_2)
  distribution_OWF_3_stage2 <- apply_multiplier(distribution_base_stage2, multiplier_tampon_3)
  
  # create new nc file
  # define the name of nc file
  nc_name <- file.path(folder_distribution, "horseMackerel.nc")
  # define the dimensions in nc files 
  dimLon <- ncdim_def( "longitude", "degree", seq(-1.95,2.45,0.1))
  dimLat <- ncdim_def( "latitude", "degree", seq(49.05,51.15,0.1))
  dimTime <- ncdim_def("time","year",seq(1,49*24,1), unlim=TRUE)
  
  # define the variable according to number of life stage
  stage0 <- ncvar_def("stage0","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9)
  stage1 <- ncvar_def("stage1","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9)
  stage2 <- ncvar_def("stage2","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9)
  
  # create nc file
  nc_output <- nc_create(nc_name, list(stage0, stage1, stage2))
  
  for (i in 1:49) { # entire simulation period
    ncvar_put(nc_output, stage0, distribution_base_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_base_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage2, distribution_base_stage2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 22:23 ) { # 2023 to 2024
    ncvar_put(nc_output, stage0, distribution_OWF_1_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_1_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage2, distribution_OWF_1_stage2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 27:28 ) { # 2028 to 2029
    ncvar_put(nc_output, stage0, distribution_OWF_2_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_2_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage2, distribution_OWF_2_stage2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 32:33 ) { # 2033 to 2034
    ncvar_put(nc_output, stage0, distribution_OWF_3_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_3_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage2, distribution_OWF_3_stage2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  
  nc_close(nc_output)
  
  
  ###### mackerel ######
  distribution_base_nc <- nc_open("osmose-eec_v4.4_yansong/Base/input/species_distribution/mackerel.nc")
  distribution_base_stage0 <- ncvar_get(distribution_base_nc, "stage0")
  distribution_base_stage1 <- ncvar_get(distribution_base_nc, "stage1")
  nc_close(distribution_base_nc)
  
  # apply the multipliers
  distribution_OWF_1_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_OWF_1)
  distribution_OWF_2_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_OWF_2)
  distribution_OWF_3_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_OWF_3)
  
  distribution_OWF_1_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_OWF_1)
  distribution_OWF_2_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_OWF_2)
  distribution_OWF_3_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_OWF_3)
  
  # create new nc file
  # define the name of nc file
  nc_name <- file.path(folder_distribution, "mackerel.nc")
  # define the dimensions in nc files 
  dimLon <- ncdim_def( "longitude", "degree", seq(-1.95,2.45,0.1))
  dimLat <- ncdim_def( "latitude", "degree", seq(49.05,51.15,0.1))
  dimTime <- ncdim_def("time","year",seq(1,49*24,1), unlim=TRUE)
  
  # define the variable according to number of life stage
  stage0 <- ncvar_def("stage0","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9, longname = "0-2 years")
  stage1 <- ncvar_def("stage1","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9, longname = "2-11 years")
  
  # create nc file
  nc_output <- nc_create(nc_name, list(stage0, stage1))
  
  for (i in 1:49) { # entire simulation period
    ncvar_put(nc_output, stage0, distribution_base_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_base_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 22:23 ) { # 2023 to 2024
    ncvar_put(nc_output, stage0, distribution_OWF_1_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_1_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 27:28 ) { # 2028 to 2029
    ncvar_put(nc_output, stage0, distribution_OWF_2_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_2_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 32:33 ) { # 2033 to 2034
    ncvar_put(nc_output, stage0, distribution_OWF_3_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_3_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  
  nc_close(nc_output)
  
  ###### sole ######
  distribution_base_nc <- nc_open("osmose-eec_v4.4_yansong/Base/input/species_distribution/sole.nc")
  distribution_base_stage0 <- ncvar_get(distribution_base_nc, "stage0")
  distribution_base_stage1 <- ncvar_get(distribution_base_nc, "stage1")
  distribution_base_stage2 <- ncvar_get(distribution_base_nc, "stage2")
  nc_close(distribution_base_nc)
  
  # apply the multipliers
  distribution_OWF_1_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_OWF_1)
  distribution_OWF_2_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_OWF_2)
  distribution_OWF_3_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_OWF_3)
  
  distribution_OWF_1_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_OWF_1)
  distribution_OWF_2_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_OWF_2)
  distribution_OWF_3_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_OWF_3)
  
  distribution_OWF_1_stage2 <- apply_multiplier(distribution_base_stage2, multiplier_OWF_1)
  distribution_OWF_2_stage2 <- apply_multiplier(distribution_base_stage2, multiplier_OWF_2)
  distribution_OWF_3_stage2 <- apply_multiplier(distribution_base_stage2, multiplier_OWF_3)
  
  # create new nc file
  # define the name of nc file
  nc_name <- file.path(folder_distribution, "sole.nc")
  # define the dimensions in nc files 
  dimLon <- ncdim_def( "longitude", "degree", seq(-1.95,2.45,0.1))
  dimLat <- ncdim_def( "latitude", "degree", seq(49.05,51.15,0.1))
  dimTime <- ncdim_def("time","year",seq(1,49*24,1), unlim=TRUE)
  
  # define the variable according to number of life stage
  stage0 <- ncvar_def("stage0","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9, longname = "0-2 years")
  stage1 <- ncvar_def("stage1","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9, longname = "2-11 years")
  stage2 <- ncvar_def("stage2","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9, longname = "2-11 years")
  
  # create nc file
  nc_output <- nc_create(nc_name, list(stage0, stage1, stage2))
  
  for (i in 1:49) { # entire simulation period
    ncvar_put(nc_output, stage0, distribution_base_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_base_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage2, distribution_base_stage2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 22:23 ) { # 2023 to 2024
    ncvar_put(nc_output, stage0, distribution_OWF_1_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_1_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage2, distribution_OWF_1_stage2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 27:28 ) { # 2028 to 2029
    ncvar_put(nc_output, stage0, distribution_OWF_2_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_2_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage2, distribution_OWF_2_stage2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 32:33 ) { # 2033 to 2034
    ncvar_put(nc_output, stage0, distribution_OWF_3_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_3_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage2, distribution_OWF_3_stage2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  
  nc_close(nc_output)
  
  
  ###### plaice ######
  distribution_base_nc <- nc_open("osmose-eec_v4.4_yansong/Base/input/species_distribution/plaice.nc")
  distribution_base_stage0 <- ncvar_get(distribution_base_nc, "stage0")
  distribution_base_stage1 <- ncvar_get(distribution_base_nc, "stage1")
  distribution_base_stage2 <- ncvar_get(distribution_base_nc, "stage2")
  nc_close(distribution_base_nc)
  
  # apply the multipliers
  distribution_OWF_1_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_OWF_1)
  distribution_OWF_2_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_OWF_2)
  distribution_OWF_3_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_OWF_3)
  
  distribution_OWF_1_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_OWF_1)
  distribution_OWF_2_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_OWF_2)
  distribution_OWF_3_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_OWF_3)
  
  distribution_OWF_1_stage2 <- apply_multiplier(distribution_base_stage2, multiplier_OWF_1)
  distribution_OWF_2_stage2 <- apply_multiplier(distribution_base_stage2, multiplier_OWF_2)
  distribution_OWF_3_stage2 <- apply_multiplier(distribution_base_stage2, multiplier_OWF_3)
  
  # create new nc file
  # define the name of nc file
  nc_name <- file.path(folder_distribution, "plaice.nc")
  # define the dimensions in nc files 
  dimLon <- ncdim_def( "longitude", "degree", seq(-1.95,2.45,0.1))
  dimLat <- ncdim_def( "latitude", "degree", seq(49.05,51.15,0.1))
  dimTime <- ncdim_def("time","year",seq(1,49*24,1), unlim=TRUE)
  
  # define the variable according to number of life stage
  stage0 <- ncvar_def("stage0","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9, longname = "0-2 years")
  stage1 <- ncvar_def("stage1","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9, longname = "2-11 years")
  stage2 <- ncvar_def("stage2","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9, longname = "2-11 years")
  
  # create nc file
  nc_output <- nc_create(nc_name, list(stage0, stage1, stage2))
  
  for (i in 1:49) { # entire simulation period
    ncvar_put(nc_output, stage0, distribution_base_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_base_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage2, distribution_base_stage2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 22:23 ) { # 2023 to 2024
    ncvar_put(nc_output, stage0, distribution_OWF_1_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_1_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage2, distribution_OWF_1_stage2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 27:28 ) { # 2028 to 2029
    ncvar_put(nc_output, stage0, distribution_OWF_2_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_2_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage2, distribution_OWF_2_stage2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 32:33 ) { # 2033 to 2034
    ncvar_put(nc_output, stage0, distribution_OWF_3_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_3_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage2, distribution_OWF_3_stage2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  
  nc_close(nc_output)
  
  
  ###### cuttlefish ######
  distribution_base_nc <- nc_open("osmose-eec_v4.4_yansong/Base/input/species_distribution/cuttlefish.nc")
  distribution_base_stage0 <- ncvar_get(distribution_base_nc, "stage0")
  distribution_base_stage1 <- ncvar_get(distribution_base_nc, "stage1")
  nc_close(distribution_base_nc)
  
  # apply the multipliers
  distribution_OWF_1_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_OWF_1)
  distribution_OWF_2_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_OWF_2)
  distribution_OWF_3_stage0 <- apply_multiplier(distribution_base_stage0, multiplier_OWF_3)
  
  distribution_OWF_1_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_OWF_1)
  distribution_OWF_2_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_OWF_2)
  distribution_OWF_3_stage1 <- apply_multiplier(distribution_base_stage1, multiplier_OWF_3)
  
  # create new nc file
  # define the name of nc file
  nc_name <- file.path(folder_distribution, "cuttlefish.nc")
  # define the dimensions in nc files 
  dimLon <- ncdim_def( "longitude", "degree", seq(-1.95,2.45,0.1))
  dimLat <- ncdim_def( "latitude", "degree", seq(49.05,51.15,0.1))
  dimTime <- ncdim_def("time","year",seq(1,49*24,1), unlim=TRUE)
  
  # define the variable according to number of life stage
  stage0 <- ncvar_def("stage0","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9, longname = "0-2 years")
  stage1 <- ncvar_def("stage1","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9, longname = "2-11 years")
  
  # create nc file
  nc_output <- nc_create(nc_name, list(stage0, stage1))
  
  for (i in 1:49) { # entire simulation period
    ncvar_put(nc_output, stage0, distribution_base_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_base_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 22:23 ) { # 2023 to 2024
    ncvar_put(nc_output, stage0, distribution_OWF_1_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_1_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 27:28 ) { # 2028 to 2029
    ncvar_put(nc_output, stage0, distribution_OWF_2_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_2_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 32:33 ) { # 2033 to 2034
    ncvar_put(nc_output, stage0, distribution_OWF_3_stage0, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
    ncvar_put(nc_output, stage1, distribution_OWF_3_stage1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  
  nc_close(nc_output)
  
  ###### herring ######
  distribution_base_nc <- nc_open("osmose-eec_v4.4_yansong/Base/input/species_distribution/herring.nc")
  distribution_base <- ncvar_get(distribution_base_nc, "stage0")
  nc_close(distribution_base_nc)
  
  # apply the multipliers for noise
  distribution_OWF_1 <- apply_multiplier(distribution_base, multiplier_tampon_1)
  distribution_OWF_2 <- apply_multiplier(distribution_base, multiplier_tampon_2)
  distribution_OWF_3 <- apply_multiplier(distribution_base, multiplier_tampon_3)
  
  # apply the multipliers for sediment suspension
  distribution_OWF_1 <- apply_multiplier(distribution_OWF_1, multiplier_OWF_1)
  distribution_OWF_2 <- apply_multiplier(distribution_OWF_1, multiplier_OWF_2)
  distribution_OWF_3 <- apply_multiplier(distribution_OWF_1, multiplier_OWF_3)
  
  
  # create new nc file
  # define the name of nc file
  nc_name <- file.path(folder_distribution, "herring.nc")
  # define the dimensions in nc files 
  dimLon <- ncdim_def( "longitude", "degree", seq(-1.95,2.45,0.1))
  dimLat <- ncdim_def( "latitude", "degree", seq(49.05,51.15,0.1))
  dimTime <- ncdim_def("time","year",seq(1,49*24,1), unlim=TRUE)
  
  # define the variable according to number of life stage
  stage0 <- ncvar_def("stage0","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9)
  
  # create nc file
  nc_output <- nc_create(nc_name, stage0)
  
  for (i in 1:49) { # entire simulation period
    ncvar_put(nc_output, stage0, distribution_base, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 22:23 ) { # 2023 to 2024
    ncvar_put(nc_output, stage0, distribution_OWF_1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 27:28 ) { # 2028 to 2029
    ncvar_put(nc_output, stage0, distribution_OWF_2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 32:33 ) { # 2033 to 2034
    ncvar_put(nc_output, stage0, distribution_OWF_3, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  
  nc_close(nc_output)
  
  ###### sardine ######
  distribution_base_nc <- nc_open("osmose-eec_v4.4_yansong/Base/input/species_distribution/sardine.nc")
  distribution_base <- ncvar_get(distribution_base_nc, "stage0")
  nc_close(distribution_base_nc)
  
  # apply the multipliers for noise
  distribution_OWF_1 <- apply_multiplier(distribution_base, multiplier_tampon_1)
  distribution_OWF_2 <- apply_multiplier(distribution_base, multiplier_tampon_2)
  distribution_OWF_3 <- apply_multiplier(distribution_base, multiplier_tampon_3)
  
  # apply the multipliers for sediment suspension
  distribution_OWF_1 <- apply_multiplier(distribution_OWF_1, multiplier_OWF_1)
  distribution_OWF_2 <- apply_multiplier(distribution_OWF_1, multiplier_OWF_2)
  distribution_OWF_3 <- apply_multiplier(distribution_OWF_1, multiplier_OWF_3)
  
  
  # create new nc file
  # define the name of nc file
  nc_name <- file.path(folder_distribution, "sardine.nc")
  # define the dimensions in nc files 
  dimLon <- ncdim_def( "longitude", "degree", seq(-1.95,2.45,0.1))
  dimLat <- ncdim_def( "latitude", "degree", seq(49.05,51.15,0.1))
  dimTime <- ncdim_def("time","year",seq(1,49*24,1), unlim=TRUE)
  
  # define the variable according to number of life stage
  stage0 <- ncvar_def("stage0","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9)
  
  # create nc file
  nc_output <- nc_create(nc_name, stage0)
  
  for (i in 1:49) { # entire simulation period
    ncvar_put(nc_output, stage0, distribution_base, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 22:23 ) { # 2023 to 2024
    ncvar_put(nc_output, stage0, distribution_OWF_1, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 27:28 ) { # 2028 to 2029
    ncvar_put(nc_output, stage0, distribution_OWF_2, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  for (i in 32:33 ) { # 2033 to 2034
    ncvar_put(nc_output, stage0, distribution_OWF_3, start = c(1,1,(i-1)*24+1), count = c(-1,-1,24))
  }
  
  nc_close(nc_output)
}
