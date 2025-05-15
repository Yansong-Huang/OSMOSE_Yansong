# ANOVA mortality scenario cost*no closure
# Auteur : Yansong Huang
# Date de création : 2024-05-12

library(dplyr)
library(broom)      # to tidy results
library(car)  # levene test

mortality_all <- readRDS("indicators/mortality_base_OWF.rds")

mortality_pred <- mortality_all %>%
  filter(species_name %in% c("cuttlefish", "herring", "plaice", "redMullet")) %>%
  filter(source == "Mpred") %>%
  filter(period == "2023-2034")

mortality_f <- mortality_all %>%
  filter(species_name %in% c("cuttlefish", "herring", "plaice", "redMullet")) %>%
  filter(source == "F") %>%
  filter(period == "2023-2034")

results_f <- mortality_f %>%
  group_by(species_name) %>%
  do({
    # 方差齐性检验
    levene <- leveneTest(mean_mortality ~ scenario, data = .)
    levene_p <- levene[1, "Pr(>F)"]
    
    # 根据方差齐性选择 t 检验或 Welch 检验
    var_equal <- levene_p > 0.05
    ttest <- t.test(mean_mortality ~ scenario, data = ., var.equal = var_equal)
    
    # 输出整理
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
  mutate(significant = t_p_value < 0.05)  # 标记显著差异的物种
