# ----------------------------------------------
# 本脚本分析“风电部署与渔业管控情境”对不同物种渔获量（yield）是否产生显著影响。
# 方法包括方差齐性检验（Levene）和条件 t 检验/Welch 检验。
# This script analyzes whether fishery yield for each species significantly differs 
# under offshore wind deployment and fisheries regulation scenarios. The workflow 
# includes Levene’s test for homogeneity of variance and conditional t or Welch test.
# ----------------------------------------------

# ANOVA yield scenario: cost * no closure

library(dplyr)
library(broom)      # 整理检验结果 / To tidy statistical outputs
library(car)        # 方差齐性检验 / For Levene's test

# 读取渔获量数据（2035-2049年平均）
# Read yield data (mean over 2035–2049)
yield_after <- readRDS("indicators/yield_by_species_after.rds")

# 排除渔业重要性较低的物种
# Remove low-relevance species
yield_after <- yield_after %>%
  filter(!species_name %in% c("poorCod", "dragonet"))

# 对每个物种进行分析
# Perform test for each species
results_after <- yield_after %>%
  group_by(species_name) %>%
  do({
    # 方差齐性检验 / Levene's test
    levene <- leveneTest(mean_yield ~ scenario, data = .)
    levene_p <- levene[1, "Pr(>F)"]
    
    # 根据方差齐性选择 t 检验或 Welch 检验
    # Choose t-test or Welch test depending on variance equality
    var_equal <- levene_p > 0.05
    ttest <- t.test(mean_yield ~ scenario, data = ., var.equal = var_equal)
    
    # 输出整理 / Tidy output
    tibble(
      levene_p = levene_p,
      t_p_value = ttest$p.value,
      mean_base = mean(.$mean_yield[.$scenario == "base"]),
      mean_other = mean(.$mean_yield[.$scenario != "base"]),
      diff = mean_other - mean_base,
      var_equal = var_equal
    )
  }) %>%
  ungroup() %>%
  # 标记具有显著差异的物种
  # Flag species with significant differences
  mutate(significant = t_p_value < 0.05)
