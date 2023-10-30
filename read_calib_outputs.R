#######################################################################
# Source: Ghassen Halouani July 2020
# Last modified: Yansong Huang 2023-10-26
# Script to read calibration results 
# and write new calibration-parameters.csv
#######################################################################


library("calibrar")
library("dplyr")

## loading calibration results 
calibration_results <- readRDS("osmose-Yansong.results")


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
abline(v=c(300,700,1200,2200), lty = c(2,2), col = c("grey", "grey"))
text(300, 18000, "Phase 1")
text(700, 16000, "Phase 2")
text(1200, 14000, "Phase 3")
text(2200, 12000, "Phase 4")



## write the new calibration parameters
new_param <- calibration_results$phases[[4]]$par
new_param_list <- calibration_results$par # for larval mortality deviation

accessibility <- 1/(1+exp(-unlist(new_param[1:10])))
population_initialization <- 10^unlist(new_param[11:26])
additional_mortality <- 10^unlist(new_param[27:42])
larval_mortality <- 10^unlist(new_param[43:58])
larval_mortality_deviation <- exp(unlist(new_param_list[59:74]))
delta_lmax_factor <- new_param_list[75:90]
fisheries_rate_base <- new_param_list[c(91,94,97,100)] %>% unlist() %>% exp()
fisheries_rate_by_period <- new_param_list[c(92,95,98,101)] %>% unlist() %>% exp()
fisheries_selectivity <- new_param_list[c(93,96,99,102)]

calib_param <- read.table("calibration-parameters.csv", sep = ",", dec = ".", header = F)
new_calib_param <- calib_param
new_calib_param$V2 <- new_param

write.table(new_calib_param, "calibration-parameters.csv", 
            row.names = F, col.names = F, sep = ",", dec = ".", quote = F)



