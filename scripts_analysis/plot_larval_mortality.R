# ----------------------------------------------
# 本脚本用于读取鳎鱼（plaice）和鲽鱼（sole）的仔鱼死亡率数据，并将其可视化为折线图。
# This script reads larval mortality data for plaice and sole,
# truncates values above 100, and visualizes them as line plots.
# ----------------------------------------------

# 加载 ggplot2 库
# Load the ggplot2 library
library(ggplot2)

# 读取鳎鱼的死亡率数据
# Read larval mortality data for plaice
file_path_ple <- "input/Base/input/mortality/larval_mortality-plaice.csv"
lar_mor_ple <- read.csv(file_path_ple)

# 重命名列为 Time 和 Value（时间步和死亡率）
# Rename columns to Time and Value
colnames(lar_mor_ple) <- c("Time", "Value")

# 将死亡率中大于 100 的值截断为 100
# Truncate mortality values above 100
lar_mor_ple$Value <- pmin(lar_mor_ple$Value, 100)

# 绘制鳎鱼的死亡率折线图
# Plot larval mortality for plaice
ggplot(lar_mor_ple, aes(x = Time, y = Value)) +
  geom_line(color = "blue") +
  xlab("Time step") +
  ylab("Larval mortality") +
  ggtitle("Larval mortality of plaice") +
  theme_minimal()

# 读取鲽鱼的死亡率数据
# Read larval mortality data for sole
file_path_sol <- "input/Base/input/mortality/larval_mortality-sole.csv"
lar_mor_sol <- read.csv(file_path_sol)

# 重命名列为 Time 和 Value（时间步和死亡率）
# Rename columns to Time and Value
colnames(lar_mor_sol) <- c("Time", "Value")

# 将死亡率中大于 100 的值截断为 100
# Truncate mortality values above 100
lar_mor_sol$Value <- pmin(lar_mor_sol$Value, 100)

# 绘制鲽鱼的死亡率折线图
# Plot larval mortality for sole
ggplot(lar_mor_sol, aes(x = Time, y = Value)) +
  geom_line(color = "blue") +
  xlab("Time step") +
  ylab("Larval mortality") +
  ggtitle("Larval mortality of sole") +
  theme_minimal()
