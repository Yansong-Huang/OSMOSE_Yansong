# ----------------------------------------------
# 本脚本基于 t 检验或 Welch 检验，评估“无封闭区域”情景下不同物种生物量的变化是否显著，
# 检验前会先进行方差齐性检验（Levene's test）。
# This script evaluates whether biomass changes under a "no closure" scenario 
# are statistically significant for each species using t-tests or Welch's tests, 
# depending on the result of Levene's test for homogeneity of variance.
# ----------------------------------------------

# ANOVA biomass scenario cost*no closure
# Author: Yansong Huang
# Creation date: 2024-05-12

library(dplyr)
library(broom)      # 整理统计检验结果 / For tidying test outputs
library(car)        # Levene 检验 / For Levene's test

# 读取“情景期间”的生物量数据
# Read biomass data during scenario
biomass_during <- readRDS("indicators/biomass_by_species_during.rds")

# 读取“情景之后”的生物量数据
# Read biomass data after scenario
biomass_after <- readRDS("indicators/biomass_by_species_after.rds")

# 对每个物种进行检验
# Perform statistical test for each species
results_after <- biomass_after %>%
  group_by(species_name) %>%
  do({
    # 方差齐性检验（Levene's test）
    # Homogeneity of variance test
    levene <- leveneTest(mean_biomass ~ scenario, data = .)
    levene_p <- levene[1, "Pr(>F)"]
    
    # 根据方差齐性选择 t 检验或 Welch 检验
    # Choose t-test or Welch test based on variance equality
    var_equal <- levene_p > 0.05
    ttest <- t.test(mean_biomass ~ scenario, data = ., var.equal = var_equal)
    
    # 整理输出结果
    # Tidy output
    tibble(
      levene_p = levene_p,
      t_p_value = ttest$p.value,
      mean_base = mean(.$mean_biomass[.$scenario == "base"]),
      mean_other = mean(.$mean_biomass[.$scenario != "base"]),
      diff = mean_other - mean_base,
      var_equal = var_equal
    )
  }) %>%
  ungroup() %>%
  # 标记具有显著差异的物种
  # Flag statistically significant results
  mutate(significant = t_p_value < 0.05)
