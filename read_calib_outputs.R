#######################################################################
# Source: Ghassen Halouani July 2020
# Script to read calibration results 
# and write new calibration-parameters.csv
#######################################################################

library("osmose")
library("calibrar")
library("dplyr")

## loading calibration results 
calibration_results <- readRDS("osmose-Yansong-05-24.results")

opt_par = get_par(calibration_results$par, linear = TRUE)
write_osmose(opt_par, file="calibration-parameters.csv")
# delete larval mortality deviation and initialisation biomass from the file


## partial fitness by generation
p_fitness_phase1 <- calibration_results$phases[[1]]$trace$fitness
p_fitness_phase2 <- calibration_results$phases[[2]]$trace$fitness
p_fitness_phase3 <- calibration_results$phases[[3]]$trace$fitness
p_fitness_phase4 <- calibration_results$phases[[4]]$trace$fitness


## global fitness by generation
g_fitness_phase1 <- apply(p_fitness_phase1, 1, sum)
g_fitness_phase2 <- apply(p_fitness_phase2, 1, sum)
g_fitness_phase3 <- apply(p_fitness_phase3, 1, sum)
g_fitness_phase4 <- apply(p_fitness_phase4, 1, sum)

## plot fitness
g_fitness <- c(g_fitness_phase1,g_fitness_phase2, g_fitness_phase3, g_fitness_phase4)
plot(g_fitness, type = "l", bty = "l", xlab = "Generations", ylab = "Fitness")
abline(v=c(200,400,700,1100), lty = c(2,2), col = c("grey", "grey","grey"))
text(100, 60000, "Phase 1")
text(300, 60000, "Phase 2")
text(550, 60000, "Phase 3")
text(900, 60000, "Phase 4")

####### update catchability matrix ######
# update object conf in run_osmose script
q_fsh0 = get_par(conf, par="osmose.user.catchability.fsh0")
q_fsh0_sp0 = get_par(q_fsh0, sp=0)
q_fsh0_sp1 = get_par(q_fsh0, sp=1)
q_fsh0_sp2 = get_par(q_fsh0, sp=2)
q_fsh0_sp3 = get_par(q_fsh0, sp=3)
q_fsh0_sp5 = get_par(q_fsh0, sp=5)
q_fsh0_sp7 = get_par(q_fsh0, sp=7)
q_fsh0_sp8 = get_par(q_fsh0, sp=8)
q_fsh0_sp9 = get_par(q_fsh0, sp=9)
q_fsh0_sp10 = get_par(q_fsh0, sp=10)
q_fsh0_sp13 = get_par(q_fsh0, sp=13)
q_fsh0_sp14 = get_par(q_fsh0, sp=14)
q_fsh0_sp15 = get_par(q_fsh0, sp=15)

q_fsh1 = get_par(conf, par="osmose.user.catchability.fsh1")
q_fsh1_sp9 = get_par(q_fsh1, sp=9)
q_fsh1_sp10 = get_par(q_fsh1, sp=10)
q_fsh1_sp11 = get_par(q_fsh1, sp=11)
q_fsh1_sp12 = get_par(q_fsh1, sp=12)

q_fsh2 = get_par(conf, par="osmose.user.catchability.fsh2")
q_fsh2_sp0 = get_par(q_fsh2, sp=0)
q_fsh2_sp5 = get_par(q_fsh2, sp=5)
q_fsh2_sp7 = get_par(q_fsh2, sp=7)
q_fsh2_sp8 = get_par(q_fsh2, sp=8)

q_fsh3 = get_par(conf, par="osmose.user.catchability.fsh3")
q_fsh3_sp0 = get_par(q_fsh3, sp=0)
q_fsh3_sp1 = get_par(q_fsh3, sp=1)
q_fsh3_sp2 = get_par(q_fsh3, sp=2)
q_fsh3_sp3 = get_par(q_fsh3, sp=3)
q_fsh3_sp5 = get_par(q_fsh3, sp=5)
q_fsh3_sp7 = get_par(q_fsh3, sp=7)
q_fsh3_sp8 = get_par(q_fsh3, sp=8)
q_fsh3_sp9 = get_par(q_fsh3, sp=9)
q_fsh3_sp10 = get_par(q_fsh3, sp=10)
q_fsh3_sp11 = get_par(q_fsh3, sp=11)
q_fsh3_sp12 = get_par(q_fsh3, sp=12)
q_fsh3_sp13 = get_par(q_fsh3, sp=13)
q_fsh3_sp14 = get_par(q_fsh3, sp=14)
q_fsh3_sp15 = get_par(q_fsh3, sp=15)

# 2. read the catchability matrix
catchability.matrix = read.csv(file="osmose-eec_v4.4_yansong/input/fishing/eec_fisheries_catchability.csv", check.names = FALSE, row.names = 1)

# 3. modify the catchability matrix
catchability.matrix[1,1] = q_fsh0_sp0
catchability.matrix[2,1] = q_fsh0_sp1
catchability.matrix[3,1] = q_fsh0_sp2
catchability.matrix[4,1] = q_fsh0_sp3
catchability.matrix[6,1] = q_fsh0_sp5
catchability.matrix[8,1] = q_fsh0_sp7
catchability.matrix[9,1] = q_fsh0_sp8
catchability.matrix[10,1] = q_fsh0_sp9
catchability.matrix[11,1] = q_fsh0_sp10
catchability.matrix[14,1] = q_fsh0_sp13
catchability.matrix[15,1] = q_fsh0_sp14
catchability.matrix[16,1] = q_fsh0_sp15

catchability.matrix[10,2] = q_fsh1_sp9
catchability.matrix[11,2] = q_fsh1_sp10
catchability.matrix[12,2] = q_fsh1_sp11
catchability.matrix[13,2] = q_fsh1_sp12

catchability.matrix[1,3] = q_fsh2_sp0
catchability.matrix[6,3] = q_fsh2_sp5
catchability.matrix[8,3] = q_fsh2_sp7
catchability.matrix[9,3] = q_fsh2_sp8

catchability.matrix[1,4] = q_fsh3_sp0
catchability.matrix[2,4] = q_fsh3_sp1
catchability.matrix[3,4] = q_fsh3_sp2
catchability.matrix[4,4] = q_fsh3_sp3
catchability.matrix[6,4] = q_fsh3_sp5
catchability.matrix[8,4] = q_fsh3_sp7
catchability.matrix[9,4] = q_fsh3_sp8
catchability.matrix[10,4] = q_fsh3_sp9
catchability.matrix[11,4] = q_fsh3_sp10
catchability.matrix[12,4] = q_fsh3_sp11
catchability.matrix[13,4] = q_fsh3_sp12
catchability.matrix[14,4] = q_fsh3_sp13
catchability.matrix[15,4] = q_fsh3_sp14
catchability.matrix[16,4] = q_fsh3_sp15

# 4. write modified catchability matrix
write.csv(catchability.matrix, file="eec_fisheries_catchability.csv")

####### update larval mortality and/or additional mortality ######

# Define species list
species_list <- c("lesserSpottedDogfish", "redMullet", "pouting", "whiting", "poorCod", "cod", "dragonet", "sole", "plaice", "horseMackerel", "mackerel", "herring", "sardine", "squids", "cuttlefish", "thornbackRay")
m.larval.deviate.all.species <- get_par(conf, "osmose.user.larval.deviate")
m.larval.constant.all.species <- get_par(conf, "mortality.additional.larva.rate")

# 1. Load calibration outputs and perform operations for each species
for (i in 0:15) {
  # Load calibration outputs
  m.larval.deviate <- get_par(m.larval.deviate.all.species, sp=i)
  m.larval.constant <- get_par(m.larval.constant.all.species, sp=i)
  
  # 2. Interpolate mortality deviates by time step
  year <- seq(1, 21)
  time_step_per_year <- seq(1, 21, length.out = 480)
  m.larval.deviate.dt <- spline(year, m.larval.deviate, xout = time_step_per_year)
  
  # 3. Calculate annual mortality
  m.larval.dt <- exp(m.larval.deviate.dt$y) * m.larval.constant / 24
  
  # 4. Write the new modified mortality vector
  write.csv(m.larval.dt, file = paste0("larval_mortality-", species_list[i + 1], ".csv"))
}

