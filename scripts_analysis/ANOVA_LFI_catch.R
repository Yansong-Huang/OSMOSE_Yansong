# ANOVA of LFI catch 40
# Auteur : Yansong Huang
# Date de création : 2024-11-21

library(tidyr)
library(dplyr)
library(purrr)
library(ncdf4)
library(patchwork)
library(car)

# variable globales
deployment_scenarios <- c("cout","protection","loin","equilibre")
regulation_scenarios <- c("sans_fermeture","fermeture_chalut","fermeture_totale")
n_years_cut <- c(10,21,22,34,35,49)
n_replicate <- 30

LFI <- function(data_yield_size) {
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
  # calculate mean LFI of all years
  lfi_mean = mean(lfi_series, na.rm = TRUE)
  
  return(lfi_mean)
}


process_LFI <- function(current_results_path, cut_off_year_begin, cut_off_year_end) {
  # 获取文件列表
  list_yield_base <- list.files(results_path_base, "Yansong_yieldDistribBySize_Simu.*csv", full.names = TRUE)
  list_yield_current <- list.files(current_results_path, "Yansong_yieldDistribBySize_Simu.*csv", full.names = TRUE)
  
  # 计算LFI
  lfi_relative <- lapply(1:n_replicate, function(simulation) {
    # 读取生物量数据
    yield_brut_base <- read.csv(list_yield_base[simulation], skip = 1)
    yield_brut_current <- read.csv(list_yield_current[simulation], skip = 1)
    
    # 筛选时间段
    yield_base_filtered <- yield_brut_base %>% filter(Time > cut_off_year_begin & Time < cut_off_year_end)
    yield_current_filtered <- yield_brut_current %>% filter(Time > cut_off_year_begin & Time < cut_off_year_end)
    
    # 计算LFI
    lfi_base <- LFI(yield_base_filtered)
    lfi_current <- LFI(yield_current_filtered)
    
    # 计算
    lfi_ratio <- lfi_current / lfi_base
    return(lfi_ratio)
  })
  
  # 转换为数值向量
  lfi_relative <- as.numeric(lfi_relative)
  return(lfi_relative)
}

# 初始化全局数据框
LFI_all <- data.frame()

# 遍历每个捕鱼政策情境
for (regulation in regulation_scenarios) {
  
  # 构建每个场景的路径
  results_path_1 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[1], "_", regulation), "Base", "output", "CIEM","SizeIndicators")
  results_path_2 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[2], "_", regulation), "Base", "output", "CIEM","SizeIndicators")
  results_path_3 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[3], "_", regulation), "Base", "output", "CIEM","SizeIndicators")
  results_path_4 <- file.path("outputs/results_1111", paste0("CC.ON_", deployment_scenarios[4], "_", regulation), "Base", "output", "CIEM","SizeIndicators")
  
  # 将路径合并为列表
  results_path_scenario <- list(results_path_1, results_path_2, results_path_3, results_path_4)
  
  # 基础路径
  results_path_base <- file.path("outputs/results_1111", "Base_simu", "Base", "output", "CIEM","SizeIndicators")
  
  # 计算时间段的数据
  
  LFI_after_list <- map(results_path_scenario, ~ process_LFI(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[5],
    cut_off_year_end = n_years_cut[6]
  ))

  names(LFI_after_list) <- c("cost", "protection", "distance", "balance")
  
  # 转换为数据框并添加标识
  LFI_after_table <- stack(LFI_after_list) %>%
    mutate(regulation = regulation)
  
  # 合并到全局数据框
  LFI_all <- rbind(
    LFI_all,
    LFI_after_table
  )
}
colnames(LFI_all) <- c("LFI_ratio", "deployment","regulation")

LFI_all$regulation <- factor(
  LFI_all$regulation,
  levels = c("sans_fermeture", "fermeture_chalut", "fermeture_totale"),
  labels = c("no closure", "trawlers closure", "complete closure")
)

Levene test
leveneTest(LFI_ratio ~ regulation * deployment, data = LFI_all)

#ANOVa
anova_model <- aov(LFI_ratio ~ regulation * deployment, data = LFI_all)
summary(anova_model)

shapiro.test(residuals(anova_model))


qqnorm(residuals(anova_model))
qqline(residuals(anova_model), col = "red")
plot(fitted(anova_model), residuals(anova_model))
abline(h = 0, col = "blue")

kruskal.test(LFI_ratio ~ regulation, data = LFI_all)
kruskal.test(LFI_ratio ~ deployment, data = LFI_all)

