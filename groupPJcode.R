### Packages Required
install.packages("readxl")
install.packages("e1071")
install.packages("ROCR")
install.packages("glmnet")
#install.packages("leaps")

install.packages("pls")# PCA
library(pls)

library(leaps)
library('e1071')
library('readxl')
library('ROCR')

library(glmnet)



### import data from excel

# for PC
Residential_Building_Data_Set <- read_excel("E:/HSUHK/statisticModeling/groupProject/Residential-Building-Data-Set.xlsx")



mydata = Residential_Building_Data_Set 


# Check for null
sum( is.na(mydata) ) > 0


############# 
install.packages("tidyverse")
library("tidyverse")
data_tibble <- as_tibble(mydata[,-27])
data_tibble$V.10 <- as.factor(data_tibble$V.10)
ggpairs(data_tibble, title = "Correlation between variables with respect to target", aes(color = V.10))

mydata_v2_v8 = mydata[,1:7] 
par(mar = c(2, 4, 2, 1))
par(mfrow = c(3, 3))  # Creates a 3x3 grid of subplots
# Loop through each variable and plot its distribution
for (i in 1:ncol(mydata_v2_v8)) {
  hist(mydata_v2_v8[, i], main = colnames(mydata_v2_v8)[i], col = "lightblue")
}

mydata_v11_v29 = mydata[,8:26] 
par(mar = c(1, 1, 1, 1))
par(mfrow = c(4, 5))  # Creates 5x5 grid of subplots

# Loop through each variable and plot its distribution
for (i in 1:ncol(mydata_v11_v29)) {
  hist(mydata_v11_v29[, i], main = colnames(mydata_v11_v29)[i], col = "lightblue")
}

# Reset the plotting parameters to default
par(mfrow = c(1, 1))

resid = simlpe_lm_model$residuals
fit = simlpe_lm_model$fitted.values
n = length(resid)                             # get no of points
par(mfrow=c(2,2))                             # split the window into 2x2 cells
plot(fit,resid); abline(h=0)                  # plot e(i) vs fit(i), add x-axis to plot
plot(1:n,resid); abline(h=0)                  # plot e(i) vs i, add x-axis to plot
plot(resid[1:(n-1)],resid[2:n]); abline(h=0)  # plot e(i) vs e(i-1), add x-axis
qqnorm(resid); qqline(resid) 
################

# Calculate correlation between numerical variables and target variable
correlation_matrix <- cor(mydata[,-27], mydata$V.10)
correlation_matrix
# Sort the variables with the absolute value of correlation in descending order
sorted_correlation_matrix <- correlation_matrix[order(abs(correlation_matrix[,1]), decreasing = TRUE),]
sorted_correlation_matrix 
barplot(sorted_correlation_matrix, las=2, ylab = "Correlation", main = "Correlation with Target Variable")

corV2_29_matrix <- cor(mydata[,-27], mydata$V.10)
corV2_29_matrix
# Sort the variables with the absolute value of correlation in descending order
sorted_corV2_29_matrix <- corV2_29_matrix[order(abs(corV2_29_matrix[,1]), decreasing = TRUE),]
sorted_corV2_29_matrix 
barplot(sorted_corV2_29_matrix, las=2, ylab = "Correlation", main = "Correlation with Target v11-v29 Variable")


###################
rb_Data <- read_xlsx("E:/HSUHK/statisticModeling/groupProject/Residential-Building-Data-Set.xlsx", skip = 1)
rb_Data <- data.frame(rb_Data)

# 删除缺失值
rb_Data <- rb_Data %>% drop_na() %>% unique()
# 拆分成5个表
# 获取v.2-v.8
base_data <- rb_Data[,6:12]
# 将每个lag拆分成单独的表
lag1_data <- rb_Data[,13:31]
lag2_data <- rb_Data[,32:50]
lag3_data <- rb_Data[,51:69]
lag4_data <- rb_Data[,70:88]
lag5_data <- rb_Data[,89:107]
# 制作平均值表
lag_avg <- c((lag1_data+lag2_data+lag3_data+lag4_data+lag5_data)/5)
lag_avg <- cbind(base_data, lag_avg, rb_Data[,108])
lag_avg <- data.frame(lag_avg)
# 重命名表名
colnames(lag_avg) <- c(colnames(lag_avg) <- c("V.2", "V.3", "V.4", "V.5", "V.6", "V.7", "V.8", "V.11", "V.12", "V.13", "V.14", "V.15", "V.16", "V.17", "V.18", "V.19", "V.20", "V.21", "V.22", "V.23", "V.24", "V.25", "V.26", "V.27", "V.28", "V.29", "V.9"))



###################



# Reset colomn names
colnames(mydata) = mydata[1,]
mydata = mydata[-1,]

# We will exam only the first lag for this project
# Remove Lag 2 to Lag 5
mydata = mydata[, c(seq(5,31),108, 109)]

names(mydata)
mydata = apply(mydata, 2, as.numeric )
mydata = data.frame(mydata)
class(mydata)

mydata_5lag = mydata[, c(seq(5,109))]
mydata_5lag
mydata_5lag = apply(mydata_lag, 2, as.numeric )
mydata_5lag = data.frame(mydata_lag)
# drop V.9 and V.1
mydata_v10 = mydata[ , !(names(mydata) %in% c('V.1','V.9'))]
mydata_v9 = mydata[ , !(names(mydata) %in% c('V.1','V.10'))]

mydata_5lagV9 = mydata_5lag[ , !(names(mydata_5lag) %in% c('V.1','V.10'))]

d = mydata[,1:7]  #v2-v8
pairs(d, col = "blue")   #画散点图

par(mar = c(1, 1, 1, 1))
dv29 = mydata[,9:27]  #v2-v8
pairs(dv29, col = "blue") 

# calculate correlation 
cor_matrix <- cor(d[,1:7])
cor_matrix_29 <- cor(mydata[,1:27])

# 可视化
corrplot(cor_matrix, method = "circle")

# v2-v29
set.seed(12345)
train_indices_v9 = sample(1:nrow(mydata_v9),0.7*nrow(mydata_v9),replace=F)
mydata_train_v9 = mydata_v9[train_indices,]
mydata_test_v9 = mydata_v9[-train_indices,]


# lag_avg 
set.seed(12345)
train_indices = sample(1:nrow(lag_avg),0.7*nrow(lag_avg),replace=F)
mydata_train = lag_avg[train_indices,]
mydata_test = lag_avg[-train_indices,]



simlpe_lm_model = lm(V.10 ~ V.2+V.3+V.4+V.5+V.6+V.7+V.8,mydata_train)
predictA = predict(simlpe_lm_model,mydata_test)
summary(simlpe_lm_model)

rmse_A = sqrt(mean((mydata_test$V.10 - predictA)^2))
38.5362



# find some predictors
#install.packages("leaps")
library(leaps)
result.all <-regsubsets(V.10~.,mydata_train,method = "backward",nvmax = 26)
names(result.all)

summary(result.all)
summary(result.all)$cp
summary(result.all)$bic
plot(summary(result.all)$bic,type = "l",ylab = "BIC")
# after selecting 
simlpe_lm_model2 = lm(V.10 ~ V.5+V.6+V.7,mydata_train)
predictB = predict(simlpe_lm_model2,mydata_test)
summary(predictB)


rmse_B = sqrt(mean((mydata_test$V.10 - predictB)^2))
38.3206
# Use cv.glmnet to do LASSO automatically
x=model.matrix(V.10~ V.2+V.3+V.4+V.5+V.6+V.7+V.8, mydata_train)[,-1]       # Get the design matrix. Dummy variables for categorical data are added. The column of ones (corresponding to the intercept term) is removed.
y=mydata_train$V.10

cv.out = cv.glmnet(x,y,alpha=1,nfolds = 10)
bestlam = cv.out$lambda.min
coef = predict(cv.out ,type="coefficients",s=bestlam)
coef

## after LASSO  V.4 -V.8 selected
simlpe_lm_model3 = lm(V.10 ~ V.4+V.5+V.6+V.7+V.8,mydata_train)
predictC = predict(simlpe_lm_model3,mydata_test)
summary(predictC)

rmse_C = sqrt(mean((mydata_test$V.10 - predictC)^2))
37.9502

# v.2-v.29  #########################################################

lm_V29model = lm(V.10 ~.,mydata_train)
predict_V29 = predict(lm_V29model,mydata_test)
summary(lm_V29model)

rmse_V29 = sqrt(mean((mydata_test$V.10 - predict_V29)^2))
37.6944


# Use cv.glmnet to do LASSO select from v.2-v.29
x2=model.matrix(V.10~., mydata_train)[,-1]       # Get the design matrix. Dummy variables for categorical data are added. The column of ones (corresponding to the intercept term) is removed.
y2=mydata_train$V.10

cv.out2 = cv.glmnet(x2,y2,alpha=1,nfolds = 10)
bestlam2 = cv.out2$lambda.min
coef2 = predict(cv.out2 ,type="coefficients",s=bestlam2)
coef2

## after LASSO  V.13,V.22,V.24 droped
LR_model4 = lm(V.10 ~ V.4+V.5+V.6+V.7+V.8+V.11+V.12+V.14+V.15+V.16+V.17+V.18+V.19+V.20
                      +V.21+V.23+V.25+V.26+V.27+V.28+V.29,mydata_train)
predictD = predict(LR_model4,mydata_test)
summary(predictD)

rmse_D = sqrt(mean((mydata_test$V.10 - predictD)^2))
36.1475


# PCA   5lag varibles
mydata_5lagV9_indices = sample(1:nrow(mydata_5lagV9),0.7*nrow(mydata_5lagV9),replace=F)
mydata_5lagV9_train = lag_avg[train_indices,]
mydata_5lagV9_test = lag_avg[-train_indices,]

pcr.fit=pcr(V.9~., data=mydata_5lagV9_train ,scale=TRUE, validation="CV")
summary (pcr.fit)

validationplot(pcr.fit ,val.type="MSEP")

pcr.pred=predict (pcr.fit ,mydata_5lagV9_train,ncomp =15)

rmse_PCA_5lagavg = sqrt(mean((mydata_test$V.9 - pcr.pred)^2))
45.1694


mydata_train_v9
pcrv9.fit=pcr(V.9~., data=mydata_train_v9 ,scale=TRUE, validation="CV")
summary (pcrv9.fit)

validationplot(pcrv9.fit ,val.type="MSEP")


pcr.predv9=predict (pcrv9.fit ,mydata_test_v9,ncomp =15)

rmse_PCAv29 = sqrt(mean((mydata_test$V.9 - pcr.predv9)^2))
194.245


x=model.matrix(V.9~., mydata_train_v9)[,-1]       # Get the design matrix. Dummy variables for categorical data are added. The column of ones (corresponding to the intercept term) is removed.
y=mydata_train_v9$V.9

cov(x)

result = prcomp(x,scale=T)
summary(result)
result$x



