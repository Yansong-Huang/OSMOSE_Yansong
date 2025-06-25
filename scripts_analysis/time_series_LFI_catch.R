# ----------------------------------------------
# 本脚本绘制不同情境下的 LFI（Large Fish Indicator）时间序列图，
# 计算基准与各风电+管控组合下的相对变化，进行可视化比较。
# This script plots time series of LFI (Large Fish Indicator) under
# different deployment and regulation scenarios, comparing to baseline.
# ----------------------------------------------

library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(purrr)
library(ncdf4)

# 全局变量定义 / Global variables
deployment_scenarios <- c("cout","protection","loin","equilibre")
regulation_scenarios <- c("sans_fermeture","fermeture_chalut","fermeture_totale")
year_begin <- 2002
year_begin_proj <- 2022
year_end <- 2050
n_years <- 49
cut_off_year <- 10 
n_replicate <- 30
deployment_colour_palette <- c("#8a2be2","#ff1493","#ff3800","#0892d0")
results_path_base <- file.path("outputs/results_1111","Base_simu","Base","output","CIEM","SizeIndicators")

# 计算 LFI 时间序列 / Compute LFI time series
LFI_series <- function(data_yield_size, thresholds = 40) {
  data_yield_size_40 <- data_yield_size %>%
    filter(Size >= 40)
  
  # 计算总生物量 / Total biomass
  total_yield <- data_yield_size %>%
    group_by(Time) %>%
    summarize(total_yield = sum(across(2:17)))
  
  # 计算大鱼生物量 / Biomass of large fish
  large_yield <- data_yield_size_40 %>%
    group_by(Time) %>%
    summarize(large_yield = sum(across(2:17)))
  
  # 计算 LFI 序列 / Compute time series of LFI
  lfi_series = large_yield$large_yield / total_yield$total_yield
  
  return(lfi_series)
}

# 计算基准情境下的 LFI / Compute reference LFI
list_yield_base <- list.files(results_path_base, "Yansong_yieldDistribBySize_Simu.*csv", full.names = TRUE)
lfi_base_list <- lapply(1:n_replicate, function(simulation) {
  yield_brut_base <- read.csv(list_yield_base[simulation], skip = 1)
  yield_base_filtered <- yield_brut_base %>% filter(Time >= cut_off_year)
  lfi_base <- LFI_series(yield_base_filtered)
  return(lfi_base)
})

lfi_base <- data.frame(lfi_base_list)
lfi_base_mean <- rowMeans(lfi_base)

# 处理每个情境的相对 LFI / Compute LFI ratio for scenarios
process_LFI_ratio <- function(current_results_path) {
  list_yield_current <- list.files(current_results_path, "Yansong_yieldDistribBySize_Simu.*csv", full.names = TRUE)
  
  lfi_replicates <- lapply(1:n_replicate, function(simulation) {
    yield_brut_current <- read.csv(list_yield_current[simulation], skip = 1)
    yield_current_filtered <- yield_brut_current %>% filter(Time >= cut_off_year)
    lfi_current <- LFI_series(yield_current_filtered)
    lfi_ratio <- lfi_current / lfi_base_mean
    return(lfi_ratio)
  })
  
  lfi_replicates <- data.frame(lfi_replicates)
  LFI_summary <- data.frame(
    year = 2011:2050,
    LFI_output_mean = rowMeans(lfi_replicates),
    LFI_output_sd = apply(lfi_replicates, 1, sd)
  )
  return(LFI_summary)
}

# 汇总所有情境的结果 / Prepare the data for all regulation scenarios
all_LFI_ratio_df <- list()

for (regulation in regulation_scenarios) {
  results_path_2 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[1], "_", regulation), "Base", "output", "CIEM", "SizeIndicators")
  results_path_3 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[2], "_", regulation), "Base", "output", "CIEM", "SizeIndicators")
  results_path_4 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[3], "_", regulation), "Base", "output", "CIEM", "SizeIndicators")
  results_path_5 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[4], "_", regulation), "Base", "output", "CIEM", "SizeIndicators")
  
  scenario_path <- list(results_path_2, results_path_3, results_path_4, results_path_5)
  
  all_LFI_ratio <- lapply(scenario_path, process_LFI_ratio)
  
  all_LFI_ratio_df[[regulation]] <- bind_rows(
    lapply(1:length(all_LFI_ratio), function(i) {
      df <- all_LFI_ratio[[i]]
      df$deployment <- deployment_scenarios[i]
      df$regulation <- regulation
      return(df)
    })
  )
}

# 合并所有数据 / Combine all data
combined_LFI_ratio_df <- bind_rows(all_LFI_ratio_df)

combined_LFI_ratio_df$regulation <- factor(
  combined_LFI_ratio_df$regulation,
  levels = c("sans_fermeture", "fermeture_chalut", "fermeture_totale"),
  labels = c("no closure", "trawlers closure", "complete closure")
)

combined_LFI_ratio_df$deployment <- factor(
  combined_LFI_ratio_df$deployment,
  levels = c("cout", "protection", "loin", "equilibre"),
  labels = c("Cost minisation", "Exclusion from environmental protection zones",
             "Long distance from the coast", "Balance")
)

# 绘图 / Plot
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
  facet_wrap(~ regulation, ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())

# 显示图像 / Display
print(combined_plot)

# 保存图像 / Save
ggsave("figures/publication/time_series/LFI_catch.png", combined_plot, width = 8, height = 8, dpi = 600)
