head(data_for_regression,5)
#scatterplot
pairs(~.,data = NumericCoupon)
NumericCoupon=NewCoupon[,c(2:6,16:25)]
NumericCoupon[is.na(NumericCoupon)]=0
head(NumericCoupon,5)
Coupon=data_for_regression[,1:6]
model_original=lm(PURCHASE_NUMBER~VALIDPERIOD+DISPPERIOD+DISCOUNT+PRICE_RATE+DISCOUNT_PRICE+DISCOUNT_PRICE_square, data = Coupon)
summary(model_original)
pairs(~.,data = Coupon,main="Simple Scatterplot Matrix")
Coupon$DISCOUNT_square=Coupon$DISCOUNT^2
Coupon$DISCOUNT_PRICE_square=Coupon$DISCOUNT_PRICE^2
Coupon$PRICE_RATE_tran=3/Coupon$PRICE_RATE
Coupon$DISCOUNT_tran=1/Coupon$DISCOUNT
Coupon$DISCOUNT_PRICE_tran=1/Coupon$DISCOUNT_PRICE
head(Coupon,3)
Coupon_new=Coupon[,c(4:9)]
head(Coupon_new)
pairs(~.,data = Coupon_new,main="Simple Scatterplot Matrix")
model=lm(PURCHASE_NUMBER~DISCOUNT_square+PRICE_RATE_tran+VALIDPERIOD+DISPPERIOD+DISCOUNT_PRICE_square,data= Coupon_new)
summary(model)
cor(NumericCoupon)
cor.test(NumericCoupon)
screeplot(NumericCoupon)
#correlation plot
library(corrplot)
corrplot(corr, method="circle")
#forward selection
head(data_for_regression,3)
Coupon_small_area=data_for_regression[,1:100]
head(Coupon_small_area)
Coupon_perfecture=data_for_regression[,c(1:45,75:147)]
head(Coupon_perfecture)
Coupon_large_area=data_for_regression[,c(1:45,148:156)]
head(Coupon_large_area)
model_small=lm(PURCHASE_NUMBER~., data = Coupon_small_area)
summary(model_small)
model_large=lm(PURCHASE_NUMBER~., data = Coupon_large_area)
summary(model_large)
model_perfecture=lm(PURCHASE_NUMBER~., data = Coupon_perfecture)
summary(model_perfecture)
library(MASS)
min.model = lm(PURCHASE_NUMBER ~ 1,data=Coupon_large_area)
biggest = formula(lm(PURCHASE_NUMBER~.,data=Coupon_large_area))
fwd.model = step(min.model, direction='forward', scope=biggest)
fullmodel=lm(PURCHASE_NUMBER~., data = Coupon_large_area)
backward.model=step(fullmodel, direction = "backward" ) 
step_model= step(fullmodel, direction="both")
forwardfit=lm(PURCHASE_NUMBER ~ en_genre_Gift.card + DISCOUNT_PRICE + DISPPERIOD + 
  VALIDPERIOD + PRICE_RATE + en_genre_Hotel.and.Japanese.hotel + 
  en_genre_Delivery.service + en_genre_Food + en_genre_Leisure + 
  en_large_area_Chugoku + en_large_area_East.Sea + en_large_area_Hokushinetsu + 
  en_large_area_Kyushu.Okinawa + en_genre_Lesson + en_genre_Health.and.medical + 
  USABLE_DATE_FRI_1 + en_large_area_Kansai + en_genre_Hair.salon + 
  USABLE_DATE_SUN_0 + USABLE_DATE_HOLIDAY_1, data=Coupon_large_area)
summary(forwardfit)
backwardfit=lm(PURCHASE_NUMBER ~ PRICE_RATE + DISCOUNT_PRICE + DISPPERIOD + 
                 VALIDPERIOD + USABLE_DATE_FRI_1 + USABLE_DATE_SUN_1 + USABLE_DATE_HOLIDAY_1 + 
                 en_genre_Delivery.service + en_genre_Food + en_genre_Gift.card + 
                 en_genre_Hair.salon + en_genre_Health.and.medical + en_genre_Hotel.and.Japanese.hotel + 
                 en_genre_Leisure + en_genre_Lesson + en_large_area_Chugoku + 
                 en_large_area_East.Sea + en_large_area_Hokushinetsu + en_large_area_Kansai + 
                 en_large_area_Kyushu.Okinawa, data=Coupon_large_area)
summary(backwardfit)
stepfit=lm(PURCHASE_NUMBER ~ PRICE_RATE + DISCOUNT_PRICE + DISPPERIOD + 
             VALIDPERIOD + USABLE_DATE_FRI_1 + USABLE_DATE_SUN_1 + USABLE_DATE_HOLIDAY_1 + 
             en_genre_Delivery.service + en_genre_Food + en_genre_Gift.card + 
             en_genre_Hair.salon + en_genre_Health.and.medical + en_genre_Hotel.and.Japanese.hotel + 
             en_genre_Leisure + en_genre_Lesson + en_large_area_Chugoku + 
             en_large_area_East.Sea + en_large_area_Hokushinetsu + en_large_area_Kansai + 
             en_large_area_Kyushu.Okinawa
           ,data = Coupon_large_area)
summary(stepfit)
#Attempt min max normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

mm_Coupon_large_area <- as.data.frame(lapply(Coupon_large_area , normalize))
head(mm_Coupon_large_area,5)
#run regression with normalized data
mm_Coupon_large_fit=lm(PURCHASE_NUMBER~., data = mm_Coupon_large_area)
summary(mm_Coupon_large_fit)
#KNN
library(class)
set.seed(123) 
test <- 1:1000
train.mm_Coupon_large <- mm_Coupon_large_area[-test,]
test.mm_Coupon_large <- mm_Coupon_large_area[test,]
install.packages(rknn)
install.packages(gmp)
library(gmp)
library(rknn)
rknn(train.mm_Coupon_large, test.mm_Coupon_large, train.mm_Coupon_large$PURCHASE_NUMBER, k=1,r=500, mtry=trunc(sqrt(ncol(train.mm_Coupon_large))), cluster=NULL, seed=NULL)
#KNN regression
library(lattice)
library(ggplot2)
library(caret)
knnfit=knnreg(mm_Coupon_large_area[,c(1:5,7:54)],mm_Coupon_large_area[,6],k=3)
summary(knnfit)

predictions=predict(knnfit,mm_Coupon_large_area[,c(1:5,7:54)])
rmse=mean((mm_Coupon_large_area$PURCHASE_NUMBER-predictions)^2)
SSE=sum((mm_Coupon_large_area$PURCHASE_NUMBER-predictions)^2)
TSS=sum((mm_Coupon_large_area$PURCHASE_NUMBER-mean(mm_Coupon_large_area$PURCHASE_NUMBER))^2)
r.squared=1-SSE/TSS
r.squared
#k=2
knnfit=knnreg(mm_Coupon_large_area[,c(1:5,7:54)],mm_Coupon_large_area[,6],k=2)
summary(knnfit)
predictions=predict(knnfit,mm_Coupon_large_area[,c(1:5,7:54)])
rmse=mean((mm_Coupon_large_area$PURCHASE_NUMBER-predictions)^2)
TSS=sum((mm_Coupon_large_area$PURCHASE_NUMBER-mean(mm_Coupon_large_area$PURCHASE_NUMBER))^2)
r.squared=1-SSE/TSS
r.squared
#k=1
knnfit=knnreg(mm_Coupon_large_area[,c(1:5,7:54)],mm_Coupon_large_area[,6],k=1)
summary(knnfit)
predictions=predict(knnfit,mm_Coupon_large_area[,c(1:5,7:54)])
SSE=sum((mm_Coupon_large_area$PURCHASE_NUMBER-predictions)^2)
TSS=sum((mm_Coupon_large_area$PURCHASE_NUMBER-mean(mm_Coupon_large_area$PURCHASE_NUMBER))^2)
r.squared=1-SSE/TSS
r.squared
#overfitting for k=1
knnfit=knnreg(train.mm_Coupon_large[,c(1:5,7:54)],train.mm_Coupon_large[,6],k=8)
predictions=predict(knnfit,test.mm_Coupon_large[,c(1:5,7:54)])
summary(predictions)
SSE=sum((test.mm_Coupon_large$PURCHASE_NUMBER - predictions)^2)
TSS=sum((test.mm_Coupon_large$PURCHASE_NUMBER - mean(test.mm_Coupon_large$PURCHASE_NUMBER))^2)
r.squared=1-SSE/TSS
r.squared
#support vector regressionlibrary(kernlab)
library(kernlab)
SVMfit=ksvm(mm_Coupon_large_area$PURCHASE_NUMBER~.,mm_Coupon_large_area)
summary(SVMfit)
predictions_svm=predict(SVMfit,mm_Coupon_large_area)
SSE_svm=sum((mm_Coupon_large_area$PURCHASE_NUMBER-predictions_svm)^2)
TSS=sum((mm_Coupon_large_area$PURCHASE_NUMBER-mean(mm_Coupon_large_area$PURCHASE_NUMBER))^2)
r.squared_svm=1-SSE_svm/TSS
r.squared_svm
#KNN but weighten the points through the distance
install.packages(kknn)
library(kknn)
help("kknn")

#regression tree
library(rpart)
regression_tree=rpart(PURCHASE_NUMBER~.,method = "anova",data=Coupon_small_area)
printcp(regression_tree)#display the results
plotcp(regression_tree)#visualize cross-validation results
summary(regression_tree)
par(mfrow=c(1,2))#two plots on one page
rsq.rpart(regression_tree)
plot(regression_tree, uniform=TRUE,main="regression tree for small area")
text(regression_tree,use.n = TRUE, all=TRUE, cex=.6)
post(regression_tree, file = "c:/Users/txu15/downloads/tree2.ps", title = "Regression Tree for small area ")
prune_regression_treefit=prune(regression_tree,cp=0.001)
plot(prune_regression_treefit,unifom=TRUE,main="pruned regression tree for small area")
text(prune_regression_treefit,use.n = TRUE, all=TRUE,cex=.6)
printcp(prune_regression_treefit)
plotcp(prune_regression_treefit)
summary(prune_regression_treefit)
par(mfrow=c(1,2))
rsq.rpart(prune_regression_treefit)
#perfecture_regression tree
library(rpart)
regression_tree=rpart(PURCHASE_NUMBER~.,method = "anova",data=Coupon_perfecture)
printcp(regression_tree)#display the results
plotcp(regression_tree)#visualize cross-validation results
summary(regression_tree)
par(mfrow=c(1,2))#two plots on one page
rsq.rpart(regression_tree)
plot(regression_tree, uniform=TRUE,main="regression tree for small area")
text(regression_tree,use.n = TRUE, all=TRUE, cex=.6)
post(regression_tree, file = "c:/Users/txu15/downloads/tree2.ps", title = "Regression Tree for small area ")
prune_regression_treefit=prune(regression_tree,cp=0.001)
plot(prune_regression_treefit,unifom=TRUE,main="pruned regression tree for small area")
text(prune_regression_treefit,use.n = TRUE, all=TRUE,cex=.6)
printcp(prune_regression_treefit)
plotcp(prune_regression_treefit)
summary(prune_regression_treefit)
par(mfrow=c(1,2))
rsq.rpart(prune_regression_treefit)

#cosine similarity

