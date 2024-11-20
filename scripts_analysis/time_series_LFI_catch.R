# Séries temporelles de capture totale
# Auteur : Yansong Huang
# Date de création : 2024-10-22

library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(purrr)
library(ncdf4)

# variables globales
deployment_scenarios <- c("cout","protection","loin","equilibre")
regulation_scenarios <- c("sans_fermeture","fermeture_chalut","fermeture_totale")
year_begin <- 2002
year_begin_proj <- 2022
year_end <- 2050
n_years <- 49
cut_off_year <- 10 
n_replicate <- 30
LFI_sd_colour_palette <- c("#8a2be2","#ff1493","#ff3800","#0892d0")
LFI_mean_colour_palette <- c("#8a2be2","#8a2be2","#ff1493","#ff1493","#ff3800","#ff3800","#0892d0","#0892d0")

LFI_series <- function(data_yield_size, thresholds = 40) {
  data_yield_size_40 <- data_yield_size %>%
    filter(Size >= 40)
  # 求总生物量和大鱼生物量
  # 按时间汇总所有物种和体型的生物量总和
  total_yield <- data_yield_size %>%
    group_by(Time) %>%
    summarize(total_yield = sum(across(2:17)))
  
  # 按时间汇总所有物种和体型的生物量总和
  large_yield <- data_yield_size_40 %>%
    group_by(Time) %>%
    summarize(large_yield = sum(across(2:17)))
  
  # calculate a time serie of LFI
  lfi_series = large_yield$large_yield / total_yield$total_yield

  return(lfi_series)
}

process_LFI <- function(current_results_path) {
  list_yield_base <- list.files(results_path_base, "Yansong_yieldDistribBySize_Simu.*csv", full.names = TRUE)
  list_yield_current <- list.files(current_results_path, "Yansong_yieldDistribBySize_Simu.*csv", full.names = TRUE)
  
  # 计算LFI
  lfi_replicates <- lapply(1:n_replicate, function(simulation) {
    # 读取生物量数据
    yield_brut_base <- read.csv(list_yield_base[simulation], skip = 1)
    yield_brut_current <- read.csv(list_yield_current[simulation], skip = 1)
    
    # 筛选时间段
    yield_base_filtered <- yield_brut_base %>% filter(Time >= cut_off_year)
    yield_current_filtered <- yield_brut_current %>% filter(Time >= cut_off_year)
    
    # 计算LFI
    lfi_base <- LFI_series(yield_base_filtered)
    lfi_current <- LFI_series(yield_current_filtered)
    
    # 计算
    lfi_ratio <- lfi_current / lfi_base
    return(lfi_ratio)
  })
  
  lfi_replicates <- data.frame(lfi_replicates)
  # species_list <- c("lesserSpottedDogfish", "redMullet", "pouting", "whiting", "poorCod", "cod", "dragonet", "sole", "plaice", "horseMackerel", "mackerel", "herring", "sardine", "squids", "cuttlefish", "thornbackRay")
  LFI_summary <-  data.frame(
    year = 2011:2050,
    LFI_output_mean = rowMeans(lfi_replicates),
    LFI_output_sd = apply(lfi_replicates, 1, sd)
  )
  return(LFI_summary)
}


# chemin pour tous les résultats
for(regulation in regulation_scenarios){
  results_path_1 <- file.path("outputs/results_1111","Base_simu","Base","output","CIEM","SizeIndicators")
  results_path_2 <- file.path("outputs/results_1111",paste0("CC.ON_",deployment_scenarios[1],"_",regulation),"Base","output","CIEM","SizeIndicators")
  results_path_3 <- file.path("outputs/results_1111",paste0("CC.ON_",deployment_scenarios[2],"_",regulation),"Base","output","CIEM","SizeIndicators")
  results_path_4 <- file.path("outputs/results_1111",paste0("CC.ON_",deployment_scenarios[3],"_",regulation),"Base","output","CIEM","SizeIndicators")
  results_path_5 <- file.path("outputs/results_1111",paste0("CC.ON_",deployment_scenarios[4],"_",regulation),"Base","output","CIEM","SizeIndicators")
  
  scenario_path <- list(results_path_2, results_path_3, results_path_4, results_path_5)
  
  # Chargement les résultats de sortie du modèle
  all_LFI_ratio <- list()
  
  # apply to all scenarios
  all_LFI_ratio <- lapply(scenario_path, process_LFI)
  
  relative_LFI_plot <- ggplot() +
    geom_line(data = all_LFI_ratio[[1]], aes(x = year, y = LFI_output_mean, color = "mean cost")) +
    geom_ribbon(data = all_LFI_ratio[[1]], aes(x = year,
                                                 ymin = LFI_output_mean - LFI_output_sd,
                                                 ymax = LFI_output_mean + LFI_output_sd,
                                                 fill = "sd cost"),
                alpha = 0.2) +
    geom_line(data = all_LFI_ratio[[2]], aes(x = year, y = LFI_output_mean, color = "mean protection")) +
    geom_ribbon(data = all_LFI_ratio[[2]], aes(x = year,
                                                 ymin = LFI_output_mean - LFI_output_sd,
                                                 ymax = LFI_output_mean + LFI_output_sd,
                                                 fill = "sd protection"),
                alpha = 0.2) +
    geom_line(data = all_LFI_ratio[[3]], aes(x = year, y = LFI_output_mean, color = "mean distance")) +
    geom_ribbon(data = all_LFI_ratio[[3]], aes(x = year,
                                                 ymin = LFI_output_mean - LFI_output_sd,
                                                 ymax = LFI_output_mean + LFI_output_sd,
                                                 fill = "sd distance"),
                alpha = 0.2) +
    geom_line(data = all_LFI_ratio[[4]], aes(x = year, y = LFI_output_mean, color = "mean balance")) +
    geom_ribbon(data = all_LFI_ratio[[4]], aes(x = year,
                                                 ymin = LFI_output_mean - LFI_output_sd,
                                                 ymax = LFI_output_mean + LFI_output_sd,
                                                 fill = "sd balance"),
                alpha = 0.2) +
    geom_hline(yintercept = 1, color = "black", linetype = "dotted")+
    geom_rect(aes(xmin = 2023, xmax = 2025, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.5) +
    geom_rect(aes(xmin = 2028, xmax = 2030, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.5) +
    geom_rect(aes(xmin = 2033, xmax = 2035, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.5) +
    scale_color_manual(name = element_blank(),
                       values = LFI_mean_colour_palette,
                       breaks = c("mean cost","sd cost","mean protection","sd protection","mean distance","sd distance", "mean balance", "sd balance"),
                       labels = c("mean cost","sd cost","mean protection","sd protection","mean distance","sd distance", "mean balance", "sd balance")) +
    scale_fill_manual(name = element_blank(),
                      values = LFI_sd_colour_palette,
                      breaks = c("sd cost","sd protection","sd distance","sd balance"),
                      labels = c("sd cost","sd protection","sd distance","sd balance")) +
    ylab("LFI catch ratio") +
    ggtitle(regulation)+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.background = element_rect(fill = "white"),
          legend.title = element_blank())
  
  print(relative_LFI_plot)
  # Sauvegarder le graphique de comparaison
  ggsave(file.path("figures/publication/time_series",regulation,"LFI_catch.png", sep=""), relative_LFI_plot, width = 10, height = 5, dpi = 600)
}