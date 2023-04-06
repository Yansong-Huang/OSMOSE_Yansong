
# library(devtools)
# install_github("osmose-model/osmose", force = TRUE)
# usethis::edit_r_environ()

library(osmose)
java_path <- "C:/Users/yhuang/Documents/OSMOSE/code/osmose_4.3.2.jar"
run_osmose("eec_all-parameters.csv", osmose=java_path, version = "4.3.2")
