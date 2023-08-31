
# library(devtools)
# install_github("osmose-model/osmose", force = TRUE)
# usethis::edit_r_environ()

library(osmose)
# library(ggplot2)
library(calibrar)

#java_path   = "R:/sync/Github/osmose-private-ricardo/inst/java"
java_path   = "C:/Users/yhuang/Documents/OSMOSE/osmose-private/inst/java"
jar_file    = file.path(java_path, "osmose_4.4.0-jar-with-dependencies.jar")
version    = "4.4.0"

config_dir  = "osmose-eec_v4.4_yansong"
main_file = "eec_all-parameters.R"

simulation = "Yansong"

config_file = file.path(config_dir, main_file)
output_dir  = file.path(config_dir, "output", simulation)

conf = read_osmose(input=config_file)

run_osmose(input = config_file, osmose=jar_file, version = "4.4.0",
           output = output_dir)

out = initialize_osmose(input=config_file, type="internannual", 
                        osmose = jar_file, version=version, append=FALSE)

run_osmose(input = config_file, osmose=jar_file, version = "4.4.0",
           output = output_dir)
# calibration set-up

calibration_path = osmose_calibration_setup(input=config_file, osmose=jar_file, version = "4.4.0", type = "survey")
osmose_calibration_test(calibration_path)

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






