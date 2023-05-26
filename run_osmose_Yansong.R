
# library(devtools)
# install_github("osmose-model/osmose", force = TRUE)
# usethis::edit_r_environ()

library(osmose)
# library(ggplot2)
library(calibrar)

java_path   = "R:/sync/Github/osmose-private-ricardo/inst/java"
java_path   = "C:/Users/yhuang/Documents/OSMOSE/osmose-private/inst/java"
jar_file    = file.path(java_path, "osmose_4.4.0-jar-with-dependencies.jar")
version    = "4.4.0"

config_dir  = "osmose-eec_v4.4_yansong"
main_file = "eec_all-parameters.R"


simulation = "Yansong"

config_file = file.path(config_dir, main_file)
output_dir  = file.path(config_dir, "output", simulation)

# éxécuter le modèles
# java_path <- "C:/Users/yhuang/Documents/OSMOSE/code/osmose_4.3.2.jar"

# update_osmose(input=config_file, osmose = jar_file, version="4.4.0")

conf = read_osmose(input=config_file)

run_osmose(input = config_file, osmose=jar_file, version = "4.4.0",
           output = output_dir)

out = initialize_osmose(input=config_file, type="internannual", 
                        osmose = jar_file, version=version, append=FALSE)

run_osmose(input = config_file, osmose=jar_file, version = "4.4.0",
           output = output_dir)

# lire les sorties
output_osmose = read_osmose(path = "output-Yansong")
plot(output_osmose)
plot(output_osmose, what="yield")

calibration_path = osmose_calibration_setup(input=config_file, osmose=jar_file)

calibration_path = osmose_calibration_setup(input=config_file, osmose=jar_file, name = "Yansong_test", data_path = "../data")


biomass = get_var(output_osmose, "biomass", expected=TRUE)
class(biomass)
dim(biomass)

biomass_list = get_var(output_osmose, "biomass", how="list")
class(biomass_list)
names(biomass_list)

# visualisation
output.dir = "figures"
plot(output_osmose, what="biomass",start=40,freq=1)
plot(output_osmose, what="yield",start=40,freq=1)






