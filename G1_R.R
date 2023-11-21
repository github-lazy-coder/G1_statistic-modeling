library(reader)
library(dplyr)
source("function1.R")
library(GGally)
install.packages("readxl")
library(readxl)


# 加载数据  
setwd("C:/Users/Administrator/Desktop/周一数据集")
data <- read_xlsx('Residential-Building-Data-Set.xlsx')
# 数据清理
data <- data %>%
  na.omit()   # 删除缺失值
# 转换为数据框
data <- as.data.frame(data)
###############################################################################

# 选择特定列
data1 <- data %>%
  select(1:12, (ncol(data)-1):ncol(data))

# 将新数据集的第一行设置为列名
colnames(data1) <- as.character(unlist(data1[1, ]))
data1 <- data1[-1, ]
#data1是V1~V10
# 转换data1中的所有列为数值型  num
data1 <- data.frame(sapply(data1, as.numeric))

str(data1)
#从data1中取V1-V10进行数据分析
d = data1[,5:14] 
pairs(d, col = "blue")#画散点图
####上面是基础数据

############################################################
# 计算每个数据框的行数
n <- 19

# 划分数据框(这里还有点问题)
df1 <- data[,1:n ]  #LAG 1
df2 <- data[,(n * 1 + 1):(n * 2)] #LAG2 
df3 <- data[,(n * 2 + 1):(n * 3)] #LAG3
df4 <- data[,(n * 3 + 1):(n * 4) ]#LAG4
df5 <- data[,(n * 4 + 1):(n * 5)] #LAG5
# 获取第一列的列名
x1 <- colnames(df1)[1]
x2 <- colnames(df2)[1]
x3 <- colnames(df3)[1]
x4 <- colnames(df4)[1]
x5 <- colnames(df5)[1]
# 将第一行设置为列名
colnames(df1) <- df1[1, ]
colnames(df2) <- df2[1, ]
colnames(df3) <- df3[1, ]
colnames(df4) <- df4[1, ]
colnames(df5) <- df5[1, ]

# 移除第一行
df1 <- df1[-1, ]
df2 <- df2[-1, ]
df3 <- df3[-1, ]
df4 <- df4[-1, ]
df5 <- df5[-1, ]

##转为num
df1 <- data.frame(sapply(df1, as.numeric))
df2 <- data.frame(sapply(df2, as.numeric))
df3 <- data.frame(sapply(df3, as.numeric))
df4 <- data.frame(sapply(df4, as.numeric))
df5 <- data.frame(sapply(df5, as.numeric))

#df1~df5是五个时间滞后数据


my_colors <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e")

pairs(df1[1:10], col = 'lightblue')  # 可以选择你喜欢的任何颜色 [1:4] 是前四行
pairs(df2, col = my_colors[2])  # 可以选择你喜欢的任何颜色没加中括号就是全部画
pairs(df3, col = my_colors[3])  # 可以选择你喜欢的任何颜色

pairs(df4, col = my_colors[4])  # 可以选择你喜欢的任何颜色
pairs(df5, col = my_colors[5])  # 可以选择你喜欢的任何颜色



###这个的用法是 改一下 df= df1 (这个df1换成你要的数据集) ，然后第147行  的[,1:4] 就是前四行 这个你也可以自己改
###整个中括号删掉就是全部

df = data1  ##data1从第五列开始才是V1  5~14  一次性不要放太多
# exonNumber <- elementNROWS(rowRanges(airway[rownames(df),]))
# df$MoreThan15Exons <- ifelse(exonNumber>15,
#                              ">15ex", "<15ex")
df$MoreThan15Exons  = '<15ex'
df[,5:14] <- log2(df[,5:14]+1)
GGally::ggpairs(df,
                5:14,
                lower = list(continuous = wrap(GGscatterPlot, method="pearson")),
                upper = list(continuous = wrap(ggally_cor, align_percent = 0.8), 
                             mapping = ggplot2::aes(color = MoreThan15Exons))) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(fill=NA),
        axis.text =  element_text(color='black'))



############################################################################################################
# 
# 计算相关系数：使用 cor() 函数。
# 可视化：使用 ggplot2 或 corrplot 包进行相关性矩阵的可视化。
##太多数据了 data1取了前12行和v9 v10 ，data2保留原数据 只是重新命名了名字 把5个时间滞后放在最后
library(ggplot2)
library(corrplot)

# 计算相关系数
cor_matrix <- cor(data1[,5:14])

# 可视化
corrplot(cor_matrix, method = "circle")



# 步骤 3: 简单线性回归
# 模型构建：使用 lm() 函数建立模型。
# 可视化：使用 ggplot2 绘制回归线和数据点。
#创建一个新的临时数据，去掉V9和前四列 因为V10 和 V9都是output 选择一个就够了 这里用V10
new_data1 <- data1 %>%
  select(-c(1,2,3,4),V.9)

# 假设 V10 是因变量，x 是自变量
simple_model <- lm(V.10~ V.1, data = new_data1)


summary(simple_model)

par(mfrow=c(3,2))  # 设置图形排列为3行2列

plot(simple_model, which=1)  # 残差与拟合值
plot(simple_model, which=2)  # 标准化残差
plot(simple_model, which=3)  # Scale-Location
plot(simple_model, which=4)  # Cook's distance
plot(simple_model, which=5)  # 残差与杠杆作用
plot(simple_model, which=6)  # Cook's distance和杠杆作用的关系

# 创建散点图
ggplot(new_data1, aes(x = V.1, y = V.10)) +
  geom_point() +               # 绘制散点图
  geom_smooth(method = "lm") +  # 添加线性拟合线
  labs(title = "Scatter Plot and Linear Fit", x = "V-9 (Independent Variable)", y = "V-9 (Dependent Variable)")



# 模型构建：同样使用 lm()，但包括多个预测变量。

# 假设 y 是因变量，x1, x2, ... 是自变量 .是全部
multi_model <- lm(V.10~ . , data = new_data1)

# 可视化
library(ggfortify)
autoplot(multi_model)


summary(multi_model)

par(mfrow=c(3,2))  # 设置图形排列为3行2列

plot(multi_model, which=1)  # 残差与拟合值
plot(multi_model, which=2)  # 标准化残差
plot(multi_model, which=3)  # Scale-Location
plot(multi_model, which=4)  # Cook's distance
plot(multi_model, which=5)  # 残差与杠杆作用
plot(multi_model, which=6)  # Cook's distance和杠杆作用的关系

