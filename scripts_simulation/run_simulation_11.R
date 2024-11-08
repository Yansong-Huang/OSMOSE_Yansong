library(osmose)

jar_file    = "osmose_4.4.0-jar-with-dependencies.jar"
version    = "4.4.0"

# define scenario names
deployment_scenarios <- c("cout","protection","loin")
# regulation_scenarios <- c("sans_fermeture","fermeture_chalut","fermeture_totale")
CC_scenarios <- c("ON","OFF")

#simulation en parallèle manuellement
regulation <- "fermeture_totale"

for (deployment in deployment_scenarios) {
  if(!dir.exists(paste0("osmose-eec_v4.4_yansong/","CC.",CC_scenarios[1],"_",deployment,"_",regulation))){
    dir.create(paste0("osmose-eec_v4.4_yansong/","CC.",CC_scenarios[1],"_",deployment,"_",regulation))
    file.copy(from = "osmose-eec_v4.4_yansong/Base",
              to = paste0("osmose-eec_v4.4_yansong/","CC.",CC_scenarios[1],"_",deployment,"_",regulation), recursive = T)
  }
  
  #replace the targeted files by their scenario sibling
  #fishing distribution map
  file.copy(file.path("data_scenarios", deployment, regulation, "fishing-distrib.nc"),
              to = paste0("osmose-eec_v4.4_yansong/","CC.",CC_scenarios[1],"_",deployment,"_",regulation,"/Base/input/fishing/fishing-distrib.nc"), overwrite = T)
  #species distribution maps
  file.copy(file.path("data_scenarios",deployment, "species_distribution"),
            to = paste0("osmose-eec_v4.4_yansong/","CC.",CC_scenarios[1],"_",deployment,"_",regulation,"/Base/input"), overwrite = T, recursive = TRUE)
  #low trophic level groups
  file.copy(from = paste0("data_scenarios/CC_",CC_scenarios[1],"/interpolated_CERES_NorthSea_2d_monthly_2002_2050.nc"),
            to = paste0("osmose-eec_v4.4_yansong/","CC.",CC_scenarios[1],"_",deployment,"_",regulation,"/Base/input/ERSEM_nc/interpolated_CERES_NorthSea_2d_monthly_2002_2022.nc"), overwrite = T)
  
  
  #run osmose
  config_dir  = paste0("osmose-eec_v4.4_yansong/","CC.",CC_scenarios[1],"_",deployment,"_",regulation,"/Base")
  main_file = "eec_all-parameters.R"
  
  simulation = "CIEM"
  
  config_file = file.path(config_dir, main_file)
  output_dir  = file.path(config_dir, "output", simulation)
  
  conf = read_osmose(input=config_file)
  
  run_osmose(input = config_file, osmose=jar_file, version = "4.4.0",
             output = output_dir)
  
}
