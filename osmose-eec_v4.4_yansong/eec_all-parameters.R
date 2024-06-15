# EEC main configuration file = 
osmose.configuration.calibration = input/calibration-parameters.csv
osmose.configuration.simulation = input/eec_param-simulation.csv
osmose.configuration.movement = input/eec_movement.R
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
simulation.nschool.sp11 = 24
simulation.nschool.sp12 = 25
simulation.nschool.sp13 = 28
simulation.nschool.sp14 = 42
simulation.nschool.sp15 = 14

osmose.version = 4.4.0

#osmose.configuration.regional = regional_outputs/regional.csv
#osmose.configuration.surveys = regional_outputs/surveys.csv
#osmose.configuration.background = eec_param-background.csv


# Survey Configuration ----------------------------------------------------

surveys.movement.netcdf.enabled = FALSE

# Survey 1 (acousticSurvey: Pelagics)
surveys.enabled.sr1 = FALSE
# surveys.name.sr1 = acousticSurvey
# 
# surveys.selectivity.type.sr1 = 1
# surveys.selectivity.l50.sr1  = 4
# surveys.selectivity.l75.sr1  = 4.5
# surveys.selectivity.tiny.sr1 = 1e-3
# 
# # surveys.targetspecies.sr1    = 0,2,3,4,5,7
# 
# surveys.movement.survey.map1      = acousticSurvey
# surveys.movement.variable.map1    = demersal
# surveys.movement.nsteps.year.map1 = 1
# surveys.movement.file.map1        = input/fishing/survey_maps.nc

# osmose.user.catchability.fsh0.sp0 = 0.315166215334695
# osmose.user.catchability.fsh0.sp1 = 0.258419743067142
# osmose.user.catchability.fsh0.sp2 = 0.482327661406563
# osmose.user.catchability.fsh0.sp3 = 0.942909114480798
# osmose.user.catchability.fsh0.sp5 = 0.205820389416546
# osmose.user.catchability.fsh0.sp7 = 0.2541038223875
# osmose.user.catchability.fsh0.sp8 = 0.427749025789491
# osmose.user.catchability.fsh0.sp9 = 0.115507533764461
# osmose.user.catchability.fsh0.sp10 = 0.99882169575186
# osmose.user.catchability.fsh0.sp13 = 0.452518685287448
# osmose.user.catchability.fsh0.sp14 = 0.345813590433097
# osmose.user.catchability.fsh0.sp15 = 0.115281829048514
# osmose.user.catchability.fsh1.sp9 = 0.119281170454459
# osmose.user.catchability.fsh1.sp10 = 0.295450395106291
# osmose.user.catchability.fsh1.sp11 = 0.985092181986801
# osmose.user.catchability.fsh1.sp12 = 0.448738354071701
# osmose.user.catchability.fsh2.sp0 = 0.179019872838496
# osmose.user.catchability.fsh2.sp5 = 0.263919775519682
# osmose.user.catchability.fsh2.sp7 = 1.06579547649055
# osmose.user.catchability.fsh2.sp8 = 0.375069751435523
# osmose.user.catchability.fsh3.sp0 = 0.17287800345159
# osmose.user.catchability.fsh3.sp1 = 0.120353178679958
# osmose.user.catchability.fsh3.sp2 = 0.206169296282231
# osmose.user.catchability.fsh3.sp3 = 0.563855230346181
# osmose.user.catchability.fsh3.sp5 = 0.1600345231266
# osmose.user.catchability.fsh3.sp7 = 0.207381493127577
# osmose.user.catchability.fsh3.sp8 = 0.246838139764309
# osmose.user.catchability.fsh3.sp9 = 0.131142435929024
# osmose.user.catchability.fsh3.sp10 = 0.632579411362316
# osmose.user.catchability.fsh3.sp11 = 1.03025517403846
# osmose.user.catchability.fsh3.sp12 = 0.437340132792056
# osmose.user.catchability.fsh3.sp13 = 0.22447863035386
# osmose.user.catchability.fsh3.sp14 = 0.147026456154422
# osmose.user.catchability.fsh3.sp15 = 0.0825934080919064





