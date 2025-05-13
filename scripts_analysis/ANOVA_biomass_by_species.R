# ANOVA biomass scenario cost*no closure
library(dplyr)
library(broom)      # to tidy results
library(car)  # levene test

biomass_during <- readRDS("indicators/biomass_by_species_during.rds")
biomass_after <- readRDS("indicators/biomass_by_species_after.rds")

results_after <- biomass_after %>%
  group_by(species_name) %>%
  do({
    # 方差齐性检验
    levene <- leveneTest(mean_biomass ~ scenario, data = .)
    levene_p <- levene[1, "Pr(>F)"]
    
    # 根据方差齐性选择 t 检验或 Welch 检验
    var_equal <- levene_p > 0.05
    ttest <- t.test(mean_biomass ~ scenario, data = ., var.equal = var_equal)
    
    # 输出整理
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
  mutate(significant = t_p_value < 0.05)  # 标记显著差异的物种
