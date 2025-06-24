# ----------------------------------------------
# 本脚本基于“风电部署和渔业管控情境”下的模拟数据，评估不同物种在情境期间的
# 死亡率（F 来源）是否发生显著变化。包括方差齐性检验（Levene）、t 检验或 Welch 检验。
# This script analyzes whether the mortality rate (source = F) of selected species
# significantly differs under offshore wind deployment and fisheries regulation 
# scenarios. The procedure includes Levene’s test for homogeneity of variance,
# followed by either a t-test or Welch test depending on the result.
# ----------------------------------------------

# ANOVA mortality scenario: cost * no closure
# Author: Yansong Huang
# Created on: 2024-05-12

library(dplyr)
library(broom)      # 整理检验结果 / For tidying test outputs
library(car)        # Levene 检验 / For Levene's test

# 读取所有死亡率数据
# Read all mortality data
mortality_all <- readRDS("indicators/mortality_base_OWF.rds")

# 选取指定物种和来源为“自然死亡率预测”（Mpred）的数据
# Select target species and source = "Mpred"
mortality_pred <- mortality_all %>%
  filter(species_name %in% c("cuttlefish", "herring", "plaice", "redMullet")) %>%
  filter(source == "Mpred") %>%
  filter(period == "2023-2034")

# 选取指定物种和来源为“渔获死亡率”（F）的数据
# Select target species and source = "F"
mortality_f <- mortality_all %>%
  filter(species_name %in% c("cuttlefish", "herring", "plaice", "redMullet")) %>%
  filter(source == "F") %>%
  filter(period == "2023-2034")

# 对每个物种进行统计检验
# Perform statistical test by species
results_f <- mortality_f %>%
  group_by(species_name) %>%
  do({
    # 方差齐性检验 / Levene's test
    levene <- leveneTest(mean_mortality ~ scenario, data = .)
    levene_p <- levene[1, "Pr(>F)"]
    
    # 根据方差齐性选择 t 检验或 Welch 检验
    # Choose test based on variance equality
    var_equal <- levene_p > 0.05
    ttest <- t.test(mean_mortality ~ scenario, data = ., var.equal = var_equal)
    
    # 输出整理 / Tidy output
    tibble(
      levene_p = levene_p,
      t_p_value = ttest$p.value,
      mean_base = mean(.$mean_mortality[.$scenario == "base"]),
      mean_other = mean(.$mean_mortality[.$scenario != "base"]),
      diff = mean_other - mean_base,
      var_equal = var_equal
    )
  }) %>%
  ungroup() %>%
  # 标记显著差异的物种
  # Flag significant results
  mutate(significant = t_p_value < 0.05)
