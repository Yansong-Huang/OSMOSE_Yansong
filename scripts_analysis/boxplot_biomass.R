# Boxplot of total biomass
# Auteur : Yansong Huang
# Date de création : 2024-10-31

library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(purrr)
library(ncdf4)
library(patchwork)

# variable globales
deployment_scenarios <- c("cout","protection","loin","equilibre")
regulation_scenarios <- c("sans_fermeture","fermeture_chalut","fermeture_totale")
CC_scenarios <- c("ON","OFF")
n_years_cut <- c(10,21,22,34,35,49)
n_replicate <- 30

process_biomass <- function(base_results_path, current_results_path,cut_off_year_begin, cut_off_year_end) {
  list_biomass_base <- list.files(base_results_path, "Yansong_biomass_Simu.", full.names = TRUE)
  list_biomass_current <- list.files(current_results_path, "Yansong_biomass_Simu.", full.names = TRUE)
  
  biomass_relative <- lapply(1:n_replicate, function(simulation) {
    biomass_brut_base <- read.csv(list_biomass_base[simulation], skip = 1)
    biomass_brut_current <- read.csv(list_biomass_current[simulation], skip = 1)
    biomass_total_base <- biomass_brut_base %>% 
      filter(Time>cut_off_year_begin)%>%
      filter(Time<cut_off_year_end)%>% 
      colMeans() %>% sum()
    biomass_total_current <- biomass_brut_current %>% 
      filter(Time>cut_off_year_begin)%>%
      filter(Time<cut_off_year_end)%>% 
      colMeans() %>% sum()
    biomass_ratio <- biomass_total_current/biomass_total_base
  })
  biomass_relative <- as.vector(biomass_relative) %>% as.numeric()
  return(biomass_relative)
}


# 初始化空列表，用于存储所有结果
total_biomass_before_list <- list()
total_biomass_during_list <- list()
total_biomass_after_list <- list()

for (regulation in regulation_scenarios){

  # 构建每个场景的路径
  results_path_1 <- file.path("outputs/results_2510", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[1], "_", regulation), "Base", "output", "CIEM")
  results_path_2 <- file.path("outputs/results_2510", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[2], "_", regulation), "Base", "output", "CIEM")
  results_path_3 <- file.path("outputs/results_2510", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[3], "_", regulation), "Base", "output", "CIEM")
  results_path_4 <- file.path("outputs/results_2510", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[4], "_", regulation), "Base", "output", "CIEM")
  
  # 基础路径
  results_path_base <- file.path("outputs/results_2510", "Base_simu", "output", "CIEM")
  # 将路径合并为列表
  results_path_scenario <- list(results_path_1, results_path_2, results_path_3, results_path_4)
  

  
  # apply the function to four deployment scenarios, respectively for three periods (before, during and after OWF construction)
  library(purrr)
  
  total_biomass_before_list <- map(results_path_scenario, ~ process_biomass(
    base_results_path = results_path_base,
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[1],
    cut_off_year_end = n_years_cut[2]
  ))
  
  total_biomass_during_list <- map(results_path_scenario, ~ process_biomass(
    base_results_path = results_path_base,
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[3],
    cut_off_year_end = n_years_cut[4]
  ))
  
  total_biomass_after_list <- map(results_path_scenario, ~ process_biomass(
    base_results_path = results_path_base,
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[5],
    cut_off_year_end = n_years_cut[6]
  ))
  
  
  # 为结果命名
  names(total_biomass_before_list) <- c("cost", "protection", "distance", "balance")
  names(total_biomass_during_list) <- c("cost", "protection", "distance", "balance")
  names(total_biomass_after_list) <- c("cost", "protection", "distance", "balance")
  
  # 将所有结果转换为数据框
  total_biomass_before_table <- stack(total_biomass_before_list)
  total_biomass_during_table <- stack(total_biomass_during_list)
  total_biomass_after_table <- stack(total_biomass_after_list)
  colnames(total_biomass_before_table) <- c("relative_biomass", "scenario")
  colnames(total_biomass_during_table) <- c("relative_biomass", "scenario")
  colnames(total_biomass_after_table) <- c("relative_biomass", "scenario")

# create plot 
biomass_boxplot_before <- ggplot(total_biomass_before_table)+
  geom_boxplot(aes(x = scenario, y = relative_biomass, fill = scenario)) +
  geom_hline(yintercept = 1, color = "black", linetype = "dotted") + 
  ggtitle("before OWF construction")+
  ylab("total biomass change")+
  ylim(0.95,1.055)+
  scale_fill_manual(
    values = c("purple", "pink", "orange", "lightblue"),
    labels = c("energy cost minimisation", "exclusion from environmental protection zones", "long distance from the coast ", "balance")) + 
  labs(fill = "deployment scenario")+
  theme_bw()+
  theme(  plot.title = element_text(size = 13),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 13),
          axis.text.x = element_text(size = 13, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 13),
          legend.title = element_text(size = 13),
          legend.text = element_text(size = 13))

biomass_boxplot_during <- ggplot(total_biomass_during_table)+
  geom_boxplot(aes(x = scenario, y = relative_biomass, fill = scenario)) +
  geom_hline(yintercept = 1, color = "black", linetype = "dotted") + 
  ggtitle("during OWF construction")+
  ylim(0.95,1.055)+
  scale_fill_manual(
    values = c("purple", "pink", "orange", "lightblue"),
    labels = c("energy cost minimisation", "exclusion from environmental protection zones", "long distance from the coast ", "balance")) + 
  labs(fill = "deployment scenario")+
  theme_bw()+
  theme(  plot.title = element_text(size = 13),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 13, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 13),
          legend.title = element_text(size = 13),
          legend.text = element_text(size = 13))

biomass_boxplot_after <- ggplot(total_biomass_after_table)+
  geom_boxplot(aes(x = scenario, y = relative_biomass, fill = scenario)) +
  geom_hline(yintercept = 1, color = "black", linetype = "dotted") + 
  ggtitle("after OWF construction")+
  ylim(0.95,1.055)+
  scale_fill_manual(
    values = c("purple", "pink", "orange", "lightblue"),
    labels = c("energy cost minimisation", "exclusion from environmental protection zones", "long distance from the coast ", "balance")) + 
  labs(fill = "deployment scenario")+
  theme_bw()+
  theme(  plot.title = element_text(size = 13),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 13, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 13),
          legend.title = element_text(size = 13),
          legend.text = element_text(size = 13))

combined_boxplot <- biomass_boxplot_before + biomass_boxplot_during + biomass_boxplot_after +
  plot_layout(guides = "collect") + 
  theme(legend.position = "right")
print(combined_boxplot)


ggsave(file.path("figures","publication","boxplot", regulation,
  "total_biomass.png"), combined_boxplot, width = 15, height = 4, dpi = 600)
                                                                                                                             
}


