# EEC main configuration file = 
osmose.configuration.simulation = eec_param-simulation.csv
osmose.configuration.movement = eec_ncdf-maps.R
osmose.configuration.mortality.fishing = eec_param-fishing.R
osmose.configuration.mortality.predation = eec_param-predation.csv
osmose.configuration.mortality.starvation = eec_param-starvation.csv
osmose.configuration.reproduction = eec_param-reproduction.csv
osmose.configuration.species = eec_param-species.csv
osmose.configuration.plankton = eec_param-ltl.csv
osmose.configuration.grid = eec_param-grid.csv
# osmose.configuration.initialization = eec_param-init-pop.csv
osmose.configuration.migration = eec_param-out-mortality.csv
osmose.configuration.output = eec_param-output.csv
osmose.configuration.mortality.additional = eec_param-additional-mortality.csv
osmose.configuration.newpar = eec-new_parameters.R
osmose.configuration.init.setup = eec_initialization_params.R
# osmose.configuration.initialization = eec-initial_conditions.osm

population.seeding.year.max = 0

# INITIALIZATION FOR FOCAL SPECIES
population.initialization.relativebiomass.enabled = TRUE
osmose.configuration.init.setup = input/init_configuration.osm
osmose.configuration.initialization = input/peru-initial_conditions.osm

mortality.subdt = 10

simulation.nschool.sp0 = 21
simulation.nschool.sp1 = 35
simulation.nschool.sp2 = 23
simulation.nschool.sp3 = 19
simulation.nschool.sp4 = 58
simulation.nschool.sp5 = 14
simulation.nschool.sp6 = 19
simulation.nschool.sp7 = 34
simulation.nschool.sp8 = 26
simulation.nschool.sp9 = 37
simulation.nschool.sp10 = 29
simulation.nschool.sp11 = 16
simulation.nschool.sp12 = 25
simulation.nschool.sp13 = 28
simulation.nschool.sp14 = 42
simulation.nschool.sp15 = 14

# Osmose 4.3.3 - Updated parameter osmose.version to 4.3.3
# Osmose 4.3.4 - Updated parameter osmose.version to 4.3.4
# Osmose 4.4.0 - Updated parameter osmose.version to 4.4.0
osmose.version = 4.4.0

#osmose.configuration.regional = regional_outputs/regional.csv
#osmose.configuration.surveys = regional_outputs/surveys.csv
#osmose.configuration.background = eec_param-background.csv
