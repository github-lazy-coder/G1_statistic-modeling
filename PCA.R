

Residential_Building_Data_Set <- read_excel("E:/HSUHK/statisticModeling/groupProject/Residential-Building-Data-Set.xlsx")

mydata = Residential_Building_Data_Set 
# Reset colomn names
colnames(mydata) = mydata[1,]
mydata = mydata[-1,]

# We will exam only the first lag for this project
# Remove Lag 2 to Lag 5
mydata = mydata[, c(seq(5,31),108, 109)]

names(mydata)
mydata = apply(mydata, 2, as.numeric )
mydata = data.frame(mydata)


# drop V.9 and V.1
mydata_v10 = mydata[ , !(names(mydata) %in% c('V.1','V.9'))]
mydata_v9 = mydata[ , !(names(mydata) %in% c('V.1','V.10'))]

#  Correlation martix
corV2_29_matrix <- cor(mydata_v9[,1:26])
corV2_29_matrix

# v2-v29
set.seed(12345)
train_indices_v9 = sample(1:nrow(mydata_v9),0.7*nrow(mydata_v9),replace=F)
mydata_train_v9 = mydata_v9[train_indices_v9,]
mydata_test_v9 = mydata_v9[-train_indices_v9,]

set.seed(12345)
train_indices_v10 = sample(1:nrow(mydata_v10),0.7*nrow(mydata_v10),replace=F)
mydata_train_v10 = mydata_v10[train_indices_v10,]
mydata_test_v10 = mydata_v10[-train_indices_v10,]


#PCA  
x=model.matrix(V.9~., mydata_train_v9)[,-1]       # Get the design matrix. Dummy variables for categorical data are added. The column of ones (corresponding to the intercept term) is removed.
y=mydata_train_v9$V.9

cov(x)

result = prcomp(x,scale=T)
summary(result)
result$x

#PCA regression  predictors
pcrv9.fit=pcr(V.9~., data=mydata_train_v9 ,scale=TRUE, validation="CV")
summary (pcrv9.fit)

validationplot(pcrv9.fit ,val.type="MSEP")


pcr.predv9=predict (pcrv9.fit ,mydata_test_v9,ncomp =15)

rmse_PCAv9 = sqrt(mean((mydata_test_v9$V.9 - pcr.predv9)^2))
193.4545
# v10
pcrv10.fit=pcr(V.10~., data=mydata_train_v10 ,scale=TRUE, validation="CV")
summary (pcrv9.fit)

validationplot(pcrv10.fit ,val.type="MSEP")

pcr.predv10=predict (pcrv10.fit ,mydata_test_v10,ncomp =15)

rmse_PCAv10 = sqrt(mean((mydata_test_v10$V.10 - pcr.predv10)^2))
40.8531






