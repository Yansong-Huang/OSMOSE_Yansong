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
LFI_sd_colour_palette <- c("#8a2be2","#0892d0","#ff3800","#ff1493")
LFI_mean_colour_palette <- c("#8a2be2","#0892d0","#ff3800","#ff1493")
results_path_base <- file.path("outputs/results_1111","Base_simu","Base","output","CIEM","SizeIndicators")

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

process_LFI_ratio <- function(current_results_path) {
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


# Prepare the data for all regulation scenarios
all_LFI_ratio_df <- list()  # Initialize an empty list to store all results
for (regulation in regulation_scenarios) {
  # Define the paths for each scenario
  results_path_2 <- file.path("outputs/results_1111",paste0("CC.ON_",deployment_scenarios[1],"_",regulation),"Base","output","CIEM","SizeIndicators")
  results_path_3 <- file.path("outputs/results_1111",paste0("CC.ON_",deployment_scenarios[2],"_",regulation),"Base","output","CIEM","SizeIndicators")
  results_path_4 <- file.path("outputs/results_1111",paste0("CC.ON_",deployment_scenarios[3],"_",regulation),"Base","output","CIEM","SizeIndicators")
  results_path_5 <- file.path("outputs/results_1111",paste0("CC.ON_",deployment_scenarios[4],"_",regulation),"Base","output","CIEM","SizeIndicators")
  
  # List of paths for processing
  scenario_path <- list(results_path_2, results_path_3, results_path_4, results_path_5)
  
  # Process LFI ratio for all scenarios
  all_LFI_ratio <- lapply(scenario_path, process_LFI_ratio)
  
  # Combine the results into one dataframe, including deployment scenario and regulation
  all_LFI_ratio_df[[regulation]] <- bind_rows(
    lapply(1:length(all_LFI_ratio), function(i) {
      df <- all_LFI_ratio[[i]]
      df$deployment <- deployment_scenarios[i]
      df$regulation <- regulation
      return(df)
    })
  )
}

# After the loop, we have all LFI data for each regulation scenario
# Combine all data into one dataframe
combined_LFI_ratio_df <- bind_rows(all_LFI_ratio_df)

combined_plot <- ggplot(combined_LFI_ratio_df) +
  geom_line(aes(x = year, y = LFI_output_mean, color = deployment)) +
  geom_ribbon(aes(x = year, ymin = LFI_output_mean - LFI_output_sd, ymax = LFI_output_mean + LFI_output_sd, fill = deployment), alpha = 0.2) +
  geom_hline(yintercept = 1, color = "black", linetype = "dotted") +
  annotate("rect", xmin = 2023, xmax = 2025, ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.5) +
  annotate("rect", xmin = 2028, xmax = 2030, ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.5) +
  annotate("rect", xmin = 2033, xmax = 2035, ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.5) +
  scale_color_manual(name = "Deployment Scenario", values = LFI_mean_colour_palette) +
  scale_fill_manual(name = "Deployment Scenario", values = LFI_sd_colour_palette) +
  ylab("LFI Catch Ratio") +
  facet_wrap(~ regulation, ncol = 1) +  # 纵向排列子图
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())

# 保存合并后的图像
ggsave("figures/publication/time_series/combined_LFI_catch.png", combined_plot, width = 8, height = 8, dpi = 600)
