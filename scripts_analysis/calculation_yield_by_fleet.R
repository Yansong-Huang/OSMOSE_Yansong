# calculation of yield by fleet for two species
# Auteur : Yansong Huang
# Date de création : 2024-05-19

# ================================================================
# 0.  依赖包 ------------------------------------------------------
# ================================================================
library(ncdf4)       # 读 netCDF
library(dplyr)       # 数据整理
library(tidyr)       # pivot_longer
library(stringr)     # 字符串清理

# ================================================================
# 1.  用户参数 ----------------------------------------------------
# ================================================================
results_dir      <- "outputs/results_1111/Base_simu/Base/output/CIEM"
pattern_nc       <- "Yansong_yieldByFishery_Simu.*\\.nc$"

species_interest <- c("redMullet", "squids")
target_years     <- 2035:2049

# ---- 时间轴相关 ----
start_year       <- 2002   # ♦ 若模型的第一个输出年份不是 2002，请改这里
nstep_per_year   <- 24     # ♦ OSMOSE 默认 15 天一步，一年 24 步

# ================================================================
# 2.  辅助函数 ----------------------------------------------------
# ================================================================
read_yield_nc_split_fleet <- function(nc_file){
  nc <- nc_open(nc_file)
  on.exit(nc_close(nc))
  
  # ---- 获取物种和渔法名 ----
  sp_att  <- ncatt_get(nc, "landings", "species_names")$value
  sp_vec  <- str_trim(strsplit(sp_att, ",")[[1]])
  fish_att <- ncatt_get(nc, "landings", "fisheries_names")$value
  fish_vec <- str_trim(strsplit(fish_att, ",")[[1]])
  
  # ---- landings: [fishery, species, time] ----
  landings <- ncvar_get(nc, "landings")
  
  sp_idx   <- match(species_interest, sp_vec)
  time_vec <- seq_len(dim(landings)[3]) - 1
  year_vec <- start_year + floor(time_vec / nstep_per_year)
  keep     <- year_vec %in% target_years
  if(!any(keep)) return(NULL)
  
  # ---- 保留目标时间段并转换为长表 ----
  df <- expand.grid(
    fishery = fish_vec,
    species = sp_vec[sp_idx],
    time = which(keep)
  )
  
  df$year  <- year_vec[keep][match(df$time, which(keep))]
  df$catch_t <- mapply(function(f, s, t) landings[f, s, t],
                       f = match(df$fishery, fish_vec),
                       s = match(df$species, sp_vec),
                       t = df$time)
  df$simulation <- basename(nc_file)
  df <- df %>% select(species, fishery, year, catch_t, simulation)
  return(df)
}

# ================================================================
# 3.  主流程 ------------------------------------------------------
# ================================================================
nc_files <- list.files(results_dir, pattern = pattern_nc, full.names = TRUE)
if(length(nc_files) == 0)
  stop("在 ", results_dir, " 没找到匹配的 .nc 文件！")

catch_all <- bind_rows(lapply(nc_files, read_yield_nc_split_fleet))
if(nrow(catch_all) == 0)
  stop("指定年份范围内没有任何数据，检查 start_year / nstep_per_year 设置？")

# ---- 按 simulation × species × fishery 对年累计 ----
catch_sum <- catch_all %>%
  group_by(simulation, species, fishery) %>%
  summarise(catch_sum_t = sum(catch_t), .groups = "drop")

# ---- 然后计算跨重复模拟的平均和标准差 ----
catch_summary <- catch_sum %>%
  group_by(species, fishery) %>%
  summarise(catch_mean_t = mean(catch_sum_t),
            catch_sd_t   = sd(catch_sum_t),
            .groups = "drop") %>%
  arrange(species, fishery)

print(catch_summary, n = Inf)
