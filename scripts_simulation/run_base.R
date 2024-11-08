library(osmose)
jar_file    = "osmose_4.4.0-jar-with-dependencies.jar"
version    = "4.4.0"

#Base simulations

config_dir  = "osmose-eec_v4.4_yansong/Base"
main_file = "eec_all-parameters.R"
 
simulation = "CIEM"
# 
config_file = file.path(config_dir, main_file)
output_dir  = file.path(config_dir, "output", simulation)
# 
conf = read_osmose(input=config_file)
 
run_osmose(input = config_file, osmose=jar_file, version = "4.4.0",
             output = output_dir)
