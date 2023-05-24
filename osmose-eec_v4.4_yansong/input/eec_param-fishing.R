# Osmose 4.3.4 - Renamed parameter fisheries.enabled into process.multispecies.fisheries.enabled
process.multispecies.fisheries.enabled = TRUE
simulation.nfisheries = 4

simulation.fishing.mortality.enabled = TRUE
fisheries.movement.netcdf.enabled = TRUE

######################### Selectivities
# Define age-selectivity for fisheries
fisheries.selectivity.l50.fsh0 = 5
fisheries.selectivity.l50.fsh1 = 5
fisheries.selectivity.l50.fsh2 = 5
fisheries.selectivity.l50.fsh3 = 5

# define constant knife-edge selectivity for species
fisheries.selectivity.type.fsh0 = 0
fisheries.selectivity.type.fsh1 = 0
fisheries.selectivity.type.fsh2 = 0
fisheries.selectivity.type.fsh3 = 0

######################################## Fbase
# Defines the mortality rate for the fisheries
fisheries.rate.base.fsh0 = 0.6
fisheries.rate.base.fsh1 = 0.3
fisheries.rate.base.fsh2 = 0.2
fisheries.rate.base.fsh3 = 0.2

fisheries.rate.byperiod.fsh0 = 1
fisheries.rate.byperiod.fsh1 = 1
fisheries.rate.byperiod.fsh2 = 1
fisheries.rate.byperiod.fsh3 = 1

######################################## Fseason

fisheries.period.number.fsh0 = 1
fisheries.period.number.fsh1 = 1
fisheries.period.number.fsh2 = 1
fisheries.period.number.fsh3 = 1

# Defaults: fisheries season equals 1
fisheries.rate.bySeason.fsh0 = 1
fisheries.rate.bySeason.fsh1 = 1
fisheries.rate.bySeason.fsh2 = 1
fisheries.rate.bySeason.fsh3 = 1

fisheries.period.start.fsh0 = 0
fisheries.period.start.fsh1 = 0
fisheries.period.start.fsh2 = 0
fisheries.period.start.fsh3 = 0


######################################## Seasonality

# Fisheries seasonality. Constants for the given season
fisheries.seasonality.fsh0 = 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166
fisheries.seasonality.fsh1 = 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166
fisheries.seasonality.fsh2 = 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166
fisheries.seasonality.fsh3 = 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166, 0.04166


######################################## Names
fisheries.name.fsh0 = bottom.trawlers
fisheries.name.fsh1 = midwater.trawlers
fisheries.name.fsh2 = netters
fisheries.name.fsh3 = others

######################################## Catchabilities
fisheries.catchability.file = fishing/eec_fisheries_catchability.csv
fisheries.discards.file = fishing/eec_discards.csv

######################################## Maps
fisheries.movement.fishery.map0 = bottom.trawlers
fisheries.movement.fishery.map1 = midwater.trawlers
fisheries.movement.fishery.map2 = netters
fisheries.movement.fishery.map3 = others

fisheries.movement.file.map0 = fishing/fishing-distrib.nc
fisheries.movement.file.map1 = fishing/fishing-distrib.nc
fisheries.movement.file.map2 = fishing/fishing-distrib.nc
fisheries.movement.file.map3 = fishing/fishing-distrib.nc

fisheries.movement.variable.map0 = fishing_area
fisheries.movement.variable.map1 = fishing_area
fisheries.movement.variable.map2 = fishing_area
fisheries.movement.variable.map3 = fishing_area

fisheries.movement.nsteps.year.map0 = 1
fisheries.movement.nsteps.year.map1 = 1
fisheries.movement.nsteps.year.map2 = 1
fisheries.movement.nsteps.year.map3 = 1



