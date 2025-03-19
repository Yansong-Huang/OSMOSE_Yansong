# créer carte de distribution de pêche pour tous les scénarios de déploiement d'éolien et réglementation de pêche
# date de création : 05/08/2024
###### read shapefiles ######
library(sf)
library(ncdf4)

if(!dir.exists("data_scenarios")){
  dir.create("data_scenarios")
}

nyears_simulation <- 49

# read base fishing effort map
nc_data <- nc_open("osmose-eec_v4.4_yansong/Base/input/fishing/fishing-distrib.nc")
fishing_area_base <- ncvar_get(nc_data, varid = "fishing_area")
nc_close(nc_data)

# define the names of OWF deployment scenarios and fishing regulation scenarios
deployment_scenarios <- c("cout","protection","loin","equilibre")
regulation_scenarios <- c("sans_fermeture","fermeture_chalut","fermeture_totale")

# function of Antione. Notice that the index grid_to_close should be coherent between the shapefile and R.
grid_closure <- function(fishing_grid,grid_owf_scenario, value){
  grid_to_close <- grid_owf_scenario
  fishing_grid_closed <- fishing_grid
  fishing_grid_closed[grid_to_close] <- value
  return(fishing_grid_closed)
}

#function for creating fishing distribution maps based on OWF deployment and fishing regulation scenarios

  
  # read attributed zones
  OWF_1_cells <- st_read(file.path("OWF_grid",paste("OWF_1.shp",sep = "")))$id
  OWF_2_cells <- st_read(file.path("OWF_grid",paste("OWF_2.shp",sep = "")))$id
  
  # define the dimensions in nc files 
  dimLon <- ncdim_def( "longitude", "degree", seq(-1.95,2.45,0.1))
  dimLat <- ncdim_def( "latitude", "degree", seq(49.05,51.15,0.1))
  dimTime <- ncdim_def("time","year",seq(1,nyears_simulation,1), unlim=FALSE)
  
  # define the variable fishing area
  fishing_area <- ncvar_def("fishing_area","", list(dimLon,dimLat,dimTime), prec = "float", missval = NaN, compression = 9)
  
  ## loop over scenarios
  for (deployment in deployment_scenarios){
    for(regulation in regulation_scenarios){
      
      # read the deployment scenario
      deployment_scenario_cells <- st_read(file.path("OWF_grid",paste("OWF_",deployment,".shp",sep = "")))$id
      
      # create folder corresponding to the input scenarios
      folder_scenario <- file.path("data_scenarios",deployment, regulation)
      if (!dir.exists(folder_scenario)){
        dir.create(folder_scenario, recursive = TRUE)
      }
      
      if (regulation == "sans_fermeture"){
        # fermer uniquement les zones en construction
        
        # define the name of nc file
        nc_name <- file.path("data_scenarios",deployment, regulation, "fishing-distrib.nc")
        # create nc file
        nc_output <- nc_create(nc_name, fishing_area)
        
        
        for (i in 1:49) { # entire simulation period
          ncvar_put(nc_output, fishing_area, fishing_area_base, start = c(1,1,i), count = c(-1,-1,1))
        }
        
        for (i in 22:23) { # modify the period from 2023 to 2024
          closure_1 <- grid_closure(fishing_area_base,  OWF_1_cells, value = 0)
          ncvar_put(nc_output, fishing_area, closure_1, start = c(1,1,i), count = c(-1,-1,1))
        }
        for (i in 27:28) {
          closure_2 <- grid_closure(fishing_area_base,  OWF_2_cells, value = 0)
          ncvar_put(nc_output, fishing_area, closure_2, start = c(1,1,i), count = c(-1,-1,1))
        }
        for (i in 32:33) {
          closure_3 <- grid_closure(fishing_area_base,  deployment_scenario_cells, value = 0)
          ncvar_put(nc_output, fishing_area, closure_3, start = c(1,1,i), count = c(-1,-1,1))
        }
        nc_close(nc_output)
        
      }else if (regulation == "fermeture_chalut"){
        # define the name of nc file
        nc_name_trawlers <- file.path("data_scenarios",deployment, regulation, "fishing-distrib-trawlers.nc")
        nc_name_netters <- file.path("data_scenarios",deployment, regulation, "fishing-distrib-netters.nc")
        # create nc file
        nc_output_trawlers <- nc_create(nc_name_trawlers, fishing_area)
        nc_output_netters <- nc_create(nc_name_netters, fishing_area)
        
        # for trawlers, fishing will be close during both construction and operation phase
        for (i in 1:49 ) { # entire simulation period
          ncvar_put(nc_output_trawlers, fishing_area, fishing_area_base, start = c(1,1,i), count = c(-1,-1,1))
        }
        for (i in 22:49) { # 2023 to 2050
          trawlers_closure_1 <- grid_closure(fishing_area_base,  OWF_1_cells, value = 0)
          ncvar_put(nc_output_trawlers, fishing_area, trawlers_closure_1, start = c(1,1,i), count = c(-1,-1,1))
        }
        for (i in 27:49) { # 2028 to 2050
          trawlers_closure_2 <- grid_closure(trawlers_closure_1,  OWF_2_cells, value = 0)
          ncvar_put(nc_output_trawlers, fishing_area, trawlers_closure_2, start = c(1,1,i), count = c(-1,-1,1))
        }
        for (i in 32:49) { # 2033 to 2050
          trawlers_closure_3 <- grid_closure(trawlers_closure_2,  deployment_scenario_cells, value = 0)
          ncvar_put(nc_output_trawlers, fishing_area, trawlers_closure_3, start = c(1,1,i), count = c(-1,-1,1))
        }
        nc_close(nc_output_trawlers)
        
        # for netters, only areas during construction will be closed
        for (i in 1:49 ) { # entire simulation period
          ncvar_put(nc_output_netters, fishing_area, fishing_area_base, start = c(1,1,i), count = c(-1,-1,1))
        }
        
        for (i in 22:23) { # 2023 to 2024 construction
          # utiliser la fonction grid_closure
          construction_closure_1 <- grid_closure(fishing_area_base,  OWF_1_cells, value = 0)
          ncvar_put(nc_output_netters, fishing_area, construction_closure_1, start = c(1,1,i), count = c(-1,-1,1))
        }
        for (i in 27:28) {
          construction_closure_2 <- grid_closure(fishing_area_base,  OWF_2_cells, value = 0)
          ncvar_put(nc_output_netters, fishing_area, construction_closure_2, start = c(1,1,i), count = c(-1,-1,1))
        }
        for (i in 32:33) {
          construction_closure_3 <- grid_closure(fishing_area_base,  deployment_scenario_cells, value = 0)
          ncvar_put(nc_output_netters, fishing_area, construction_closure_3, start = c(1,1,i), count = c(-1,-1,1))
        }
        nc_close(nc_output_netters)
        
      }else if (regulation == "fermeture_totale"){ 
        # no fishing during construction and operational phase
        
        # define the name of nc file
        nc_name <- file.path("data_scenarios",deployment, regulation, "fishing-distrib.nc")
        # create nc file
        nc_output <- nc_create(nc_name, fishing_area)
        
        
        for (i in 1:49) { # entire simulation period
          ncvar_put(nc_output, fishing_area, fishing_area_base, start = c(1,1,i), count = c(-1,-1,1))
        }
        
        for (i in 22:49) { # modify the period from 2023 to 2024
          closure_1 <- grid_closure(fishing_area_base,  OWF_1_cells, value = 0)
          ncvar_put(nc_output, fishing_area, closure_1, start = c(1,1,i), count = c(-1,-1,1))
        }
        for (i in 27:49) {
          closure_2 <- grid_closure(closure_1,  OWF_2_cells, value = 0)
          ncvar_put(nc_output, fishing_area, closure_2, start = c(1,1,i), count = c(-1,-1,1))
        }
        for (i in 32:49) {
          closure_3 <- grid_closure(closure_2,  deployment_scenario_cells, value = 0)
          ncvar_put(nc_output, fishing_area, closure_3, start = c(1,1,i), count = c(-1,-1,1))
        }
        nc_close(nc_output)
        
      }else{
        print("unknown regulation scenario!")
      }
    }
  }
  

