###########################################################################
# Run OSMOSE_EEC 
# OSMOSE 4.3.2
###########################################################################

# install OSMOSE
# library(usethis)
# usethis::edit_r_environ()

# library(devtools)
# install_github("osmose-model/osmose-private",   auth_token = "7ddaa9cb533e8a8eca0d883a378eee577a4aff31")


library("osmose")
library("beepr")

## Run the model
run_osmose("eec_all-parameters.csv", version = "4.3.2")
beep(3)





