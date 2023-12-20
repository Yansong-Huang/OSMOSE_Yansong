#######################################################################
# Source: Ghassen Halouani July 2020
# Last modified: Yansong Huang 2023-12-12
# Script to read calibration results 
# and write new calibration-parameters.csv
#######################################################################

library("osmose")
library("calibrar")
library("dplyr")

## loading calibration results 
calibration_results_test <- readRDS("calibration_simple_test3/osmose-test.results")
calibration_results <- readRDS("osmose-Yansong-12-14.results")

opt_par = get_par(calibration_results$par, linear = TRUE)
write_osmose(opt_par, file="calibration-parameters.csv")

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
g_fitness <- c(g_fitness_phase1, g_fitness_phase2, g_fitness_phase3, g_fitness_phase4)
plot(g_fitness, type = "l", bty = "l", xlab = "Generations", ylab = "Fitness")
abline(v=c(300,700,1200,1700), lty = c(2,2), col = c("grey", "grey"))
text(300, 18000, "Phase 1")
text(700, 16000, "Phase 2")
text(1200, 14000, "Phase 3")
text(1700, 12000, "Phase 4")



