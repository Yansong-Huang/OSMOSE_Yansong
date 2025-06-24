# ----------------------------------------------
# Biomass calculation per species inside Offshore Wind Farms (OWF)
# 风电场内各物种生物量计算
# Author: Yansong Huang
# Created on: 2024-08-14
# Description:
# This script calculates biomass ratios of different fish species inside offshore wind farms under various wind farm deployment and fisheries management scenarios.
# It reads spatialized biomass data (NetCDF format), filters by OWF grid cells, computes mean and standard deviation across multiple simulations, and plots time series.
# 本脚本用于计算不同风电部署和渔业管控情境下，风电场内各鱼类物种的生物量变化比例（相对于基础模拟）。
# 读取空间化生物量数据（NetCDF格式），筛选风电场网格，根据多次模拟计算均值及标准差，并绘制时序图。
# ----------------------------------------------

library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(purrr)
library(ncdf4)

source("scripts_analysis/OWF_mask.R")  # Load OWF mask matrix
# 加载风电场掩膜矩阵

# Global variables / 全局变量
deployment_scenarios <- c("cout","protection","loin","equilibre")
regulation_scenarios <- c("sans_fermeture","fermeture_chalut","fermeture_totale")
CC_scenarios <- c("ON","OFF")
year_begin <- 2002
year_begin_proj <- 2022
year_end <- 2050
n_years <- 49
cut_off_year <- 9 # Start visualization from year 2010 / 开始绘图年份为2010年
n_replicate <- 30
n_species <- 16

biomass_sd_colour_palette <- c("#8a2be2","#ff1493","#ff3800","#0892d0")
biomass_mean_colour_palette <- c("#8a2be2","#8a2be2","#ff1493","#ff1493","#ff3800","#ff3800","#0892d0","#0892d0")

# Paths for all scenarios / 所有情景的路径
regulation <- regulation_scenarios[2]
results_path_1 <- file.path("outputs/results_2510","Base_simu","output","CIEM")
results_path_2 <- file.path("outputs/results_2510",paste0("CC.",CC_scenarios[1],"_",deployment_scenarios[1],"_",regulation),"Base","output","CIEM")
results_path_3 <- file.path("outputs/results_2510",paste0("CC.",CC_scenarios[1],"_",deployment_scenarios[2],"_",regulation),"Base","output","CIEM")
results_path_4 <- file.path("outputs/results_2510",paste0("CC.",CC_scenarios[1],"_",deployment_scenarios[3],"_",regulation),"Base","output","CIEM")
results_path_5 <- file.path("outputs/results_2510",paste0("CC.",CC_scenarios[1],"_",deployment_scenarios[4],"_",regulation),"Base","output","CIEM")

scenario_path <- list(results_path_2, results_path_3, results_path_4, results_path_5)


###### Biomass processing function ######
# Process biomass for a given scenario path
# 对给定情景路径处理生物量数据
process_biomass <- function(current_results_path) {
  # List nc files for spatialized biomass for base and current scenario
  # 列出基础和当前路径下的空间化生物量NetCDF文件
  list_biomass_nc_base <- list.files(results_path_1, pattern = "Yansong_spatializedBiomass_Simu.", full.names = TRUE)
  list_biomass_nc_current <- list.files(current_results_path, pattern = "Yansong_spatializedBiomass_Simu.", full.names = TRUE)
  
  # Exclude cod (species index 6) from analysis
  # 排除鳕鱼（第6种）不做分析
  biomass_relative <- bind_rows(lapply(c(1:5,7:16), function(species_index) {
    # Use n_replicate simulations instead of fixed 10
    # 模拟次数由固定10改为变量 n_replicate
    biomass_species <- map_dfc(1:n_replicate, function(simulation) {
      # Open netCDF files
      # 打开NetCDF文件
      nc_base <- nc_open(list_biomass_nc_base[simulation])
      nc_current <- nc_open(list_biomass_nc_current[simulation])
      
      # Read "Biomass" variable
      # 读取变量"Biomass"
      biomass_base <- ncvar_get(nc_base, "Biomass")
      biomass_current <- ncvar_get(nc_current, "Biomass")
      
      nc_close(nc_base)
      nc_close(nc_current)
      
      # Subset years 2010-2050 (indices relative to cut_off_year)
      # 截取2010至2050年数据段
      biomass_base_sub <- biomass_base[,,species_index, cut_off_year:n_years]
      biomass_current_sub <- biomass_current[,,species_index, cut_off_year:n_years]
      
      # Filter cells inside OWF using mask
      # 用掩膜筛选风电场内格点
      OWF_cells_base <- list()
      OWF_cells_current <- list()
      
      for (lon in 1:45) {
        for (lat in 1:22) {
          if (!is.na(biomass_base_sub[lon, lat, 1]) && mask_OWF[lon, lat]) {
            OWF_cells_base[[length(OWF_cells_base) + 1]] <- biomass_base_sub[lon, lat, ]
            OWF_cells_current[[length(OWF_cells_current) + 1]] <- biomass_current_sub[lon, lat, ]
          }
        }
      }
      
      biomass_base_mat <- do.call(cbind, OWF_cells_base)
      biomass_current_mat <- do.call(cbind, OWF_cells_current)
      
      # Calculate mean biomass ratio over years for OWF cells
      # 计算风电场内所有格点的年均生物量比率
      rowMeans(biomass_current_mat) / rowMeans(biomass_base_mat)
    })
    
    # Species list / 物种列表
    species_list <- c("lesserSpottedDogfish", "redMullet", "pouting", "whiting", "poorCod", "cod", 
                      "dragonet", "sole", "plaice", "horseMackerel", "mackerel", "herring", 
                      "sardine", "squids", "cuttlefish", "thornbackRay")
    
    data.frame(
      year = 2010:2050,
      species_name = species_list[species_index],
      biomass_output_mean = rowMeans(biomass_species),
      biomass_output_sd = apply(biomass_species, 1, sd)
    )
  }))
  
  return(biomass_relative)
}

# Apply process_biomass to all scenarios
# 对所有情景调用处理函数
all_biomass <- lapply(scenario_path, process_biomass)

# Plotting comparison figure
# 绘制比较图
biomass_plot <- ggplot() +
  geom_line(data = all_biomass[[1]], aes(x = year, y = biomass_output_mean, color = "mean cost")) +
  geom_ribbon(data = all_biomass[[1]], aes(x = year,
                                           ymin = biomass_output_mean - biomass_output_sd,
                                           ymax = biomass_output_mean + biomass_output_sd,
                                           fill = "sd cost"),
              alpha = 0.2) +
  geom_line(data = all_biomass[[2]], aes(x = year, y = biomass_output_mean, color = "mean protection")) +
  geom_ribbon(data = all_biomass[[2]], aes(x = year,
                                           ymin = biomass_output_mean - biomass_output_sd,
                                           ymax = biomass_output_mean + biomass_output_sd,
                                           fill = "sd protection"),
              alpha = 0.2) +
  geom_line(data = all_biomass[[3]], aes(x = year, y = biomass_output_mean, color = "mean distance")) +
  geom_ribbon(data = all_biomass[[3]], aes(x = year,
                                           ymin = biomass_output_mean - biomass_output_sd,
                                           ymax = biomass_output_mean + biomass_output_sd,
                                           fill = "sd distance"),
              alpha = 0.2) +
  geom_line(data = all_biomass[[4]], aes(x = year, y = biomass_output_mean, color = "mean balance")) +
  geom_ribbon(data = all_biomass[[4]], aes(x = year,
                                           ymin = biomass_output_mean - biomass_output_sd,
                                           ymax = biomass_output_mean + biomass_output_sd,
                                           fill = "sd balance"),
              alpha = 0.2) +
  geom_hline(yintercept = 1, color = "black", linetype = "dotted") +
  annotate("rect", xmin = 2023, xmax = 2025, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "grey") +
  annotate("rect", xmin = 2028, xmax = 2030, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "grey") +
  annotate("rect", xmin = 2033, xmax = 2035, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "grey") +
  scale_color_manual(name = element_blank(),
                     values = biomass_mean_colour_palette,
                     breaks = c("mean cost","sd cost","mean protection","sd protection","mean distance","sd distance", "mean balance", "sd balance"),
                     labels = c("mean cost","sd cost","mean protection","sd protection","mean distance","sd distance", "mean balance", "sd balance")) +
  scale_fill_manual(name = element_blank(),
                    values = biomass_sd_colour_palette,
                    breaks = c("sd cost","sd protection","sd distance","sd balance"),
                    labels = c("sd cost","sd protection","sd distance","sd balance")) +
  facet_wrap(~species_name, scales = "free_y", ncol = 4) +
  ylab("biomass ratio") +
  ggtitle(paste("Biomass per species in OWF relative to reference simulation, scenario", regulation)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.title = element_blank())

print(biomass_plot)

# Save the comparison plot
# 保存比较图
ggsave(file.path("figures/publication/time_series", regulation, "OWF_biomass_by_species_2510.png"), biomass_plot, width = 15, height = 8, dpi = 600)
