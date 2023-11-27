#######################################################################
# Source: Ghassen Halouani July 2020
# Last modified: Yansong Huang 2023-10-26
# Script to read calibration results 
# and write new calibration-parameters.csv
#######################################################################


library("calibrar")
library("dplyr")

## loading calibration results 
calibration_results <- readRDS("osmose-Yansong-11-17.results")


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


## global fitness by generation
# g_fitness_phase1 <- apply(p_fitness_phase1, 1, function(x) sum(x[!is.infinite(x)]))
# g_fitness_phase2 <- apply(p_fitness_phase2, 1, function(x) sum(x[!is.infinite(x)]))
# g_fitness_phase3 <- apply(p_fitness_phase3, 1, function(x) sum(x[!is.infinite(x)]))

## plot fitness
g_fitness <- c(g_fitness_phase1, g_fitness_phase2, g_fitness_phase3, g_fitness_phase4)
plot(g_fitness, type = "l", bty = "l", xlab = "Generations", ylab = "Fitness")
abline(v=c(300,700,1200,1700), lty = c(2,2), col = c("grey", "grey"))
text(300, 18000, "Phase 1")
text(700, 16000, "Phase 2")
text(1200, 14000, "Phase 3")
text(1700, 12000, "Phase 4")



## write the new calibration parameters
new_param <- calibration_results$par 

accessibility <- 1/(1+exp(-unlist(new_param[1:10])))
population_initialization <- 10^unlist(new_param[11:20])
additional_mortality <- 10^unlist(new_param[21:36])
larval_mortality <- 10^unlist(new_param[37:52])
larval_mortality_deviation <- exp(unlist(new_param[53:68]))
delta_lmax_factor <- new_param[69:84]
fisheries_rate_base <- new_param[c(85,89,93,97)] %>% unlist() %>% exp()
fisheries_rate_by_period <- new_param[c(86,90,94,98)] %>% unlist() %>% exp()
fisheries_selectivity <- new_param[c(87,88,91,92,95,96,99,100)]


write.table(new_calib_param, "calibration-parameters.csv", 
            row.names = F, col.names = F, sep = ",", dec = ".", quote = F)



