# EEC main configuration file = 
# osmose.configuration.calibration = calibration_parameters.R
osmose.configuration.simulation = input/eec_param-simulation.csv
osmose.configuration.movement = input/eec_ncdf-maps.R
osmose.configuration.mortality.fishing = input/eec_param-fishing.R
osmose.configuration.mortality.predation = input/eec_param-predation.csv
osmose.configuration.mortality.starvation = input/eec_param-starvation.csv
osmose.configuration.reproduction = input/eec_param-reproduction.csv
osmose.configuration.species = input/eec_param-species.csv
osmose.configuration.plankton = input/eec_param-ltl.csv
osmose.configuration.grid = input/eec_param-grid.csv
osmose.configuration.seeding = input/eec-param_seeding.R
osmose.configuration.migration = input/eec_param-out-mortality.csv
osmose.configuration.output = input/eec_param-output.csv
osmose.configuration.mortality.additional = input/eec_param-additional-mortality.csv
osmose.configuration.newpar = input/eec-new_parameters.R
osmose.configuration.init.setup = input/eec_initialization_params.R
osmose.configuration.initialization = input/initial_conditions.osm

population.seeding.year.max = 20
simulation.time.start = 2002
output.step0.include = FALSE

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

osmose.version = 4.4.0

#osmose.configuration.regional = regional_outputs/regional.csv
#osmose.configuration.surveys = regional_outputs/surveys.csv
#osmose.configuration.background = eec_param-background.csv


# Survey Configuration ----------------------------------------------------

surveys.movement.netcdf.enabled = TRUE

# Survey 1 (acousticSurvey: Pelagics)
surveys.enabled.sr1 = TRUE
surveys.name.sr1 = acousticSurvey

surveys.selectivity.type.sr1 = 1
surveys.selectivity.l50.sr1  = 4
surveys.selectivity.l75.sr1  = 4.5
surveys.selectivity.tiny.sr1 = 1e-3

# surveys.targetspecies.sr1    = 0,2,3,4,5,7

surveys.movement.survey.map1      = acousticSurvey
surveys.movement.variable.map1    = demersal
surveys.movement.nsteps.year.map1 = 1
surveys.movement.file.map1        = input/fishing/survey_maps.nc

osmose.user.catchability.fsh0.sp0 = 0.5








