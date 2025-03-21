# 1. 载入必要的库
library(ggplot2)
library(dplyr)
library(corrplot)

# 2. 读取数据
data <- read.csv("/mnt/data/heart_failure.csv")

# 3. 数据概览
str(data)  # 查看数据结构
summary(data)  # 查看基本统计信息

# 4. 检查缺失值
missing_values <- colSums(is.na(data))
print(missing_values)

# 5. 变量分布可视化
# 5.1 目标变量 fatal_mi 的分布
fatal_plot <- ggplot(data, aes(x = factor(fatal_mi))) +
  geom_bar(fill = "red") +
  labs(title = "Fatal MI Distribution", x = "Fatal MI", y = "Count")
print(fatal_plot)

# 5.2 连续变量的直方图
data %>%
  select(age, creatinine_phosphokinase, ejection_fraction, platelets, serum_creatinine, serum_sodium, time) %>%
  gather(variable, value) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Histograms of Continuous Variables")

# 6. 相关矩阵
data_numeric <- data %>% select(-fatal_mi) # 去掉目标变量
cor_matrix <- cor(data_numeric)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7, tl.col = "black")
