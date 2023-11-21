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
# drop V.9 and V.1
mydata = mydata[ , !(names(mydata) %in% c('V.1','V.9'))]


set.seed(12345)
train_indices = sample(1:nrow(mydata),0.5*nrow(mydata),replace=F)
mydata_train = mydata[train_indices,]
mydata_test = mydata[-train_indices,]

simlpe_lm_model = lm(V.10 ~ V.2+V.3+V.4+V.5+V.6+V.7+V.8,mydata_train)
predictA = predict(simlpe_lm_model,mydata_test)
summary(predictA)

#predictedLR_labels <- ifelse(predictA-mydata_test$V.10, 1, 0)
#accuracy_LS <- mean(predictedLS_labels == test_data$output)

rmse_A = sqrt(mean((mydata_test$V.10 - predictA)^2))
38.5362

# find some predictors
#install.packages("leaps")
library(leaps)
result.all <-regsubsets(V.10 ~ V.2+V.3+V.4+V.5+V.6+V.7+V.8,mydata_train,method = "backward")
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
summary(predict_V29)

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


# PCA


pcr.fit=pcr(V.10~., data=mydata_train ,scale=TRUE, validation="CV")
summary (pcr.fit)

validationplot(pcr.fit ,val.type="MSEP")
pcr.pred=predict (pcr.fit ,mydata_test,ncomp =5)

rmse_PCA = sqrt(mean((mydata_test$V.10 - pcr.pred)^2))
45.1694

