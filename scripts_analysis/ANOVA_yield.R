# Boxplot of total yield
# Auteur : Yansong Huang
# Date de création : 2024-10-31

library(tidyr)
library(dplyr)
library(purrr)
library(car)

# variables globales
deployment_scenarios <- c("cout","protection","loin","equilibre")
regulation_scenarios <- c("sans_fermeture","fermeture_chalut","fermeture_totale")
CC_scenarios <- c("ON","OFF")
n_years_cut <- c(10,21,22,34,35,49)
n_replicate <- 30

process_yield <- function(current_results_path,cut_off_year_begin, cut_off_year_end) {
  list_yield_base <- list.files(results_path_base, "Yansong_yield_Simu.*csv", full.names = TRUE)
  list_yield_current <- list.files(current_results_path, "Yansong_yield_Simu.*csv", full.names = TRUE)
  
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
  return(yield_relative)
}

# 初始化全局数据框
total_yield_all <- data.frame()

# 遍历每个捕鱼政策情境
for (regulation in regulation_scenarios) {
  
  # 构建场景路径
  results_path_1 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[1], "_", regulation), "Base", "output", "CIEM")
  results_path_2 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[2], "_", regulation), "Base", "output", "CIEM")
  results_path_3 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[3], "_", regulation), "Base", "output", "CIEM")
  results_path_4 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[4], "_", regulation), "Base", "output", "CIEM")
  
  results_path_scenario <- list(results_path_1, results_path_2, results_path_3, results_path_4)
  results_path_base <- file.path("outputs/results_1111", "Base_simu", "Base", "output", "CIEM")
  
  # 分别计算三个时间段的数据
  
  total_yield_period_list <- map(results_path_scenario, ~ process_yield(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[3],
    cut_off_year_end = n_years_cut[4]
  ))

  names(total_yield_period_list) <- c("cost", "protection", "distance", "balance")
  
  # 转换为数据框并添加标识
  total_yield_period_table <- stack(total_yield_period_list) %>%
    mutate(regulation = regulation)
  
  # 合并到全局数据框
  total_yield_all <- rbind(
    total_yield_all,
    total_yield_period_table
  )
}
colnames(total_yield_all) <- c("yield_ratio", "deployment","regulation")

total_yield_all$regulation <- factor(
  total_yield_all$regulation,
  levels = c("sans_fermeture", "fermeture_chalut", "fermeture_totale"),
  labels = c("no closure", "trawlers closure", "complete closure")
)

# Levene test 
leveneTest(yield_ratio ~ regulation * deployment, data = total_yield_all)

#ANOVa
anova_model <- aov(yield_ratio ~ regulation * deployment, data = total_yield_all)
summary(anova_model)

shapiro.test(residuals(anova_model))

1-median(total_yield_all[total_yield_all$regulation=="no closure",]$yield_ratio)

