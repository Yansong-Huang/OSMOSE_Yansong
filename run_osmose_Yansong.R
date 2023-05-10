
# library(devtools)
# install_github("osmose-model/osmose", force = TRUE)
# usethis::edit_r_environ()

library(osmose)

# éxécuter le modèles
java_path <- "C:/Users/yhuang/Documents/OSMOSE/code/osmose_4.3.2.jar"
run_osmose("eec_all-parameters.csv", osmose=java_path, version = "4.3.2")

# lire les sorties
output_osmose = read_osmose("output-")
names(output_osmose)

biomass = get_var(output_osmose, "biomass", expected=TRUE)
class(biomass)
dim(biomass)

biomass_list = get_var(output_osmose, "biomass", how="list")
class(biomass_list)
names(biomass_list)

# visualisation
output.dir = "figures"
plot(output_osmose, what="yield",start=20)






