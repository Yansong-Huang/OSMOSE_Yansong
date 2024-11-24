# t test of LFI catch
# Auteur : Yansong Huang
# Date de création : 2024-11-22

library(tidyr)
library(dplyr)
library(purrr)

# variables globales
deployment_scenarios <- c("cout","protection","loin","equilibre")
regulation_scenarios <- c("sans_fermeture","fermeture_chalut","fermeture_totale")
CC_scenarios <- c("ON","OFF")
n_years_cut <- c(10,21,22,34,35,49)
n_replicate <- 30
results_path_base <- file.path("outputs/results_1111", "Base_simu", "Base", "output", "CIEM","SizeIndicators")
LFI <- function(data_yield_size, thresholds = 40) {
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
  list_yield_current <- list.files(current_results_path, "Yansong_yieldDistribBySize_Simu.*csv", full.names = TRUE)
  
  # 计算LFI
  lfi_catch <- lapply(1:n_replicate, function(simulation) {
    # 读取数据
    yield_brut_current <- read.csv(list_yield_current[simulation], skip = 1)
    # 筛选时间段
    yield_current_filtered <- yield_brut_current %>% filter(Time > cut_off_year_begin & Time < cut_off_year_end)
    # 计算LFI
    lfi_current <- LFI(yield_current_filtered)
    return(lfi_current)
  })
  
  # 转换为数值向量
  lfi_catch <- as.numeric(lfi_catch)
  return(lfi_catch)
}


# 初始化全局数据框
LFI_catch_all <- data.frame()

# 遍历每个捕鱼政策情境
for (regulation in regulation_scenarios) {
  
  # 构建场景路径
  results_path_1 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[1], "_", regulation), "Base", "output", "CIEM","SizeIndicators")
  results_path_2 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[2], "_", regulation), "Base", "output", "CIEM","SizeIndicators")
  results_path_3 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[3], "_", regulation), "Base", "output", "CIEM","SizeIndicators")
  results_path_4 <- file.path("outputs/results_1111", paste0("CC.", CC_scenarios[1], "_", deployment_scenarios[4], "_", regulation), "Base", "output", "CIEM","SizeIndicators")
  
  results_path_scenario <- list(results_path_1, results_path_2, results_path_3, results_path_4)
  
  # 计算时间段的数据
  LFI_catch_period_list <- map(results_path_scenario, ~ process_LFI(
    current_results_path = .x,
    cut_off_year_begin = n_years_cut[5],
    cut_off_year_end = n_years_cut[6]
  ))
  
  names(LFI_catch_period_list) <- c("cost", "protection", "distance", "balance")
  
  # 转换为数据框并添加标识
  LFI_catch_period_table <- stack(LFI_catch_period_list) %>%
    mutate(regulation = regulation)
  
  # 合并到全局数据框
  LFI_catch_all <- rbind(
    LFI_catch_all,
    LFI_catch_period_table
  )
}


colnames(LFI_catch_all) <- c("LFI", "deployment","regulation")

LFI_catch_all$regulation <- factor(
  LFI_catch_all$regulation,
  levels = c("sans_fermeture", "fermeture_chalut", "fermeture_totale"),
  labels = c("no closure", "trawlers closure", "complete closure")
)

# treat reference simulations
LFI_catch_base_list <- process_LFI(
  current_results_path = results_path_base,
  cut_off_year_begin = n_years_cut[5],
  cut_off_year_end = n_years_cut[6]
)
LFI_catch_base <- as.vector(LFI_catch_base_list)

LFI_catch_base_sample <- sample(LFI_catch_base, 90, replace = TRUE)
LFI_catch_base_sample_2 <- sample(LFI_catch_base, 120, replace = TRUE)
# test each deployment scenario
t.test(LFI_catch_base_sample,
       LFI_catch_all[LFI_catch_all$deployment=="cost",]$LFI)
t.test(LFI_catch_base_sample,
       LFI_catch_all[LFI_catch_all$deployment=="protection",]$LFI)
t.test(LFI_catch_base_sample,
       LFI_catch_all[LFI_catch_all$deployment=="distance",]$LFI)
t.test(LFI_catch_base_sample,
       LFI_catch_all[LFI_catch_all$deployment=="balance",]$LFI)
# test each regulation scenario
t.test(LFI_catch_base_sample_2,
       LFI_catch_all[LFI_catch_all$regulation=="no closure",]$LFI)
t.test(LFI_catch_base_sample_2,
       LFI_catch_all[LFI_catch_all$regulation=="trawlers closure",]$LFI)
t.test(LFI_catch_base_sample_2,
       LFI_catch_all[LFI_catch_all$regulation=="complete closure",]$LFI)
