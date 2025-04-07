# 加载 ggplot2 库
library(ggplot2)

# 读取数据
file_path_ple <- "osmose-eec_v4.4_yansong/Base/input/mortality/larval_mortality-plaice.csv"
lar_mor_ple <- read.csv(file_path_ple)

# 确保第二列名称正确（假设第二列名称为 "Value"，你可以用 colnames(lar_mor_ple) 检查）
colnames(lar_mor_ple) <- c("Time", "Value")

# 将第二列中大于 100 的值替换为 100
lar_mor_ple$Value <- pmin(lar_mor_ple$Value, 100)

# 绘制折线图
ggplot(lar_mor_ple, aes(x = Time, y = Value)) +
  geom_line(color = "blue") +
  xlab("Time step") +
  ylab("Larval mortality") +
  ggtitle("Larval mortality of plaice") +
  theme_minimal()

# 读取数据
file_path_sol <- "osmose-eec_v4.4_yansong/Base/input/mortality/larval_mortality-sole.csv"
lar_mor_sol <- read.csv(file_path_sol)

# 确保第二列名称正确（假设第二列名称为 "Value"，你可以用 colnames(lar_mor_sol) 检查）
colnames(lar_mor_sol) <- c("Time", "Value")

# 将第二列中大于 100 的值替换为 100
lar_mor_sol$Value <- pmin(lar_mor_sol$Value, 100)

# 绘制折线图
ggplot(lar_mor_sol, aes(x = Time, y = Value)) +
  geom_line(color = "blue") +
  xlab("Time step") +
  ylab("Larval mortality") +
  ggtitle("Larval mortality of sole") +
  theme_minimal()

