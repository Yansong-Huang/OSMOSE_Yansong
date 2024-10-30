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
regulation_scenarios <- c("sans_fermeture","fermeture_chalut")
CC_scenarios <- c("ON","OFF")
cut_off_year_begin <- 35 # 1 22 35
cut_off_year_end <- 49 # 21 34 49
n_replicate <- 10


process_biomass <- function(base_results_path, current_results_path) {
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
  #biomass_ratio <- c(biomass_ratio_mean = mean(biomass_relative),biomass_ratio_sd = sd(biomass_relative))
  return(biomass_relative)
}

process_yield <- function(base_results_path, current_results_path) {
  list_yield_base <- list.files(base_results_path, "Yansong_yield_Simu.", full.names = TRUE)
  list_yield_current <- list.files(current_results_path, "Yansong_yield_Simu.", full.names = TRUE)
  
  yield_relative <- lapply(1:n_replicate, function(simulation) {
    yield_brut_base <- read.csv(list_yield_base[simulation], skip = 1)
    yield_brut_current <- read.csv(list_yield_current[simulation], skip = 1)
    yield_total_base <- yield_brut_base %>% 
      filter(Time>cut_off_year_begin)%>%
      filter(Time<cut_off_year_end)%>% 
      colMeans() %>% sum()
    yield_total_current <- yield_brut_current %>%       
      filter(Time>cut_off_year_begin)%>%
      filter(Time<cut_off_year_end)%>% 
      colMeans() %>% sum()
    yield_ratio <- yield_total_current/yield_total_base
  })
  yield_relative <- as.vector(yield_relative) %>% as.numeric()
  #yield_ratio <- c(yield_ratio_mean = mean(yield_relative),yield_ratio_sd = sd(yield_relative))
  return(yield_relative)
}

# chemin pour tous les résultats
folder_list <- c("outputs/results_2510")
# regulation <- regulation_scenarios[2]

# 初始化空列表，用于存储所有结果
all_biomass_combined_1 <- list()

for (folder in folder_list) {
  # 构建每个场景的路径
  results_path_1 <- file.path(folder, paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[1], "_", regulation_scenarios[1]), "Base", "output", "CIEM")
  results_path_2 <- file.path(folder, paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[2], "_", regulation_scenarios[1]), "Base", "output", "CIEM")
  results_path_3 <- file.path(folder, paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[3], "_", regulation_scenarios[1]), "Base", "output", "CIEM")
  results_path_4 <- file.path(folder, paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[4], "_", regulation_scenarios[1]), "Base", "output", "CIEM")
  
  # 将路径合并为列表
  results_path <- list(results_path_1, results_path_2, results_path_3, results_path_4)
  
  # 基础路径
  results_path_base <- file.path(folder, "Base_simu", "output", "CIEM")
  
  # 对所有场景应用 `process_biomass` 函数
  all_biomass <- lapply(results_path, process_biomass, base_results_path = results_path_base)
  
  # 为结果命名
  names(all_biomass) <- c("cost", "protection", "distance", "balance")
  
  # 将结果合并到主列表中
  all_biomass_combined_1 <- c(all_biomass_combined_1, all_biomass)
}

# initialise an empty dataframe for stocking results
all_biomass_combined_2 <- list()

for (folder in folder_list) {
  # 构建每个场景的路径
  results_path_1 <- file.path(folder, paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[1], "_", regulation_scenarios[2]), "Base", "output", "CIEM")
  results_path_2 <- file.path(folder, paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[2], "_", regulation_scenarios[2]), "Base", "output", "CIEM")
  results_path_3 <- file.path(folder, paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[3], "_", regulation_scenarios[2]), "Base", "output", "CIEM")
  results_path_4 <- file.path(folder, paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[4], "_", regulation_scenarios[2]), "Base", "output", "CIEM")
  
  # 将路径合并为列表
  results_path <- list(results_path_1, results_path_2, results_path_3, results_path_4)
  
  # 基础路径
  results_path_base <- file.path(folder, "Base_simu", "output", "CIEM")
  
  # 对所有场景应用 `process_biomass` 函数
  all_biomass <- lapply(results_path, process_biomass, base_results_path = results_path_base)
  
  # 为结果命名
  names(all_biomass) <- c("cost", "protection", "distance", "balance")
  
  # 将结果合并到主列表中
  all_biomass_combined_2 <- c(all_biomass_combined_2, all_biomass)
}

# 将所有结果转换为数据框
biomass_SER_1 <- stack(all_biomass_combined_1)
colnames(biomass_SER_1) <- c("relative_biomass", "scenario")
biomass_SER_2 <- stack(all_biomass_combined_2)
colnames(biomass_SER_2) <- c("relative_biomass", "scenario")



# create plot 
plot_SER_biomass_1 <- ggplot(biomass_SER_1)+
  geom_boxplot(aes(x = scenario, y = relative_biomass, fill = scenario)) +
  geom_hline(yintercept = 1, color = "black", linetype = "dotted") + 
      ggtitle("no closure")+
      ylab("total biomass change")+
      ylim(0.95,1.04)+
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

plot_SER_biomass_2 <- ggplot(biomass_SER_2)+
  geom_boxplot(aes(x = scenario, y = relative_biomass, fill = scenario)) +
  geom_hline(yintercept = 1, color = "black", linetype = "dotted") + 
  ggtitle("trawlers closure")+
  ylim(0.95,1.04)+ 
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

combined_plot_biomass <- plot_SER_biomass_1 + plot_SER_biomass_2 + 
  plot_layout(guides = "collect") + 
  theme(legend.position = "right")
print(combined_plot_biomass)
ggsave(file.path("figures","publication","boxplot", 
"biomass_after_OWF_2510.png"), combined_plot_biomass, width = 11, height = 4, dpi = 600)

# combined_plot_yield <- plot_SER_yield_1 + plot_SER_yield_2 + 
#   plot_layout(guides = "collect") & 
#   theme(legend.position = "right")
# print(combined_plot_yield)

# ggsave(file.path("figures_CIEM_CC_old","boxplot", 
       # "combined_yield_boxplot.png"), combined_plot_yield, width = 11, height = 4, dpi = 600)

###### test statistiques ######
shapiro.test(yield_SER$relative_yield)
kruskal.test(relative_yield~scenario, data = yield_SER) 

# data:  relative_yield by scenario (trawler closure)
# Kruskal-Wallis chi-squared = 11.129, df =
#   3, p-value = 0.01105

###### normality test ######
# shapiro.test(biomass_SER_1$relative_biomass)
shapiro.test(biomass_SER_1[biomass_SER_1$scenario=="cost",]$relative_biomass)
# 0.7221
shapiro.test(biomass_SER_1[biomass_SER_1$scenario=="protection",]$relative_biomass)
# 0.3783
shapiro.test(biomass_SER_1[biomass_SER_1$scenario=="distance",]$relative_biomass)
# 0.008623
shapiro.test(biomass_SER_1[biomass_SER_1$scenario=="balance",]$relative_biomass)
# 0.07595


shapiro.test(biomass_SER_2[biomass_SER_2$scenario=="cost",]$relative_biomass)
# 0.09219
shapiro.test(biomass_SER_2[biomass_SER_2$scenario=="protection",]$relative_biomass)
# 0.5906
shapiro.test(biomass_SER_2[biomass_SER_2$scenario=="distance",]$relative_biomass)
# 0.06158
shapiro.test(biomass_SER_2[biomass_SER_2$scenario=="balance",]$relative_biomass)
# 0.4847

# Wilcoxon test for comparing SER scenarios with reference
wilcox.test(biomass_SER_1[biomass_SER_1$scenario=="cost",]$relative_biomass, mu = 1, alternative = "less")
# p-value = 1.895e-06
wilcox.test(biomass_SER_1[biomass_SER_1$scenario=="protection",]$relative_biomass, mu = 1, alternative = "less")
# p-value = 4.61e-06
wilcox.test(biomass_SER_1[biomass_SER_1$scenario=="distance",]$relative_biomass, mu = 1, alternative = "less")
# p-value = 3.955e-05
wilcox.test(biomass_SER_1[biomass_SER_1$scenario=="balance",]$relative_biomass, mu = 1, alternative = "less")
# p-value = 5.305e-06


# Kruskal-Walllis for comparing four SER scenarios
# scenario "no closure" three groups out of four meet normality
kruskal.test(relative_biomass~scenario, data = biomass_SER_1)
# 0.1461

# scenario "trawlers closure" three groups out of four meet normality
kruskal.test(relative_biomass~scenario, data = biomass_SER_2) 
# 0.003575

# Mann-Whitney test for comparing two fishing scenarios
mann_test_1 <- rbind(biomass_SER_1[biomass_SER_1$scenario=="cost",],biomass_SER_2[biomass_SER_2$scenario=="cost",])
mann_test_1$scenario <- c(rep("no",30),rep("trawl",30))
wilcox.test(relative_biomass~scenario, data = mann_test_1, paired =FALSE)
# p-value = 0.008383 alternative hypothesis: true location shift is not equal to 0

mann_test_2 <- rbind(biomass_SER_1[biomass_SER_1$scenario=="protection",],biomass_SER_2[biomass_SER_2$scenario=="protection",])
mann_test_2$scenario <- c(rep("no",30),rep("trawl",30))
wilcox.test(relative_biomass~scenario, data = mann_test_2, paired =FALSE)
# p-value = 0.007301 alternative hypothesis: true location shift is not equal to 0

mann_test_3 <- rbind(biomass_SER_1[biomass_SER_1$scenario=="distance",],biomass_SER_2[biomass_SER_2$scenario=="distance",])
mann_test_3$scenario <- c(rep("no",30),rep("trawl",30))
wilcox.test(relative_biomass~scenario, data = mann_test_3, paired =FALSE)
# p-value = 0.924

mann_test_4 <- rbind(biomass_SER_1[biomass_SER_1$scenario=="balance",],biomass_SER_2[biomass_SER_2$scenario=="balance",])
mann_test_4$scenario <- c(rep("no",30),rep("trawl",30))
wilcox.test(relative_biomass~scenario, data = mann_test_4, paired =FALSE)
# p-value = 0.1229

