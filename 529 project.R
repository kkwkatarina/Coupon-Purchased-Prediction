Coupon=Calculated_Coupon_List[,-c(2:6,10,11,13,14)]
head(Coupon,5)
area.f = factor(Coupon$en_large_area)
dummies = model.matrix(~area.f)
head(dummies,5)
summary(Coupon$en_large_area)
#replace 
Coupon[, 7:15][is.na(Coupon[, 7:15])] <- 1
CouponNew[ ,7:15][CouponNew[ ,7:15]== NA] =1 # why does not work
head(Coupon,5)
#final code to insert: all dummies=2 means area.Chugoku
newcol=dummies[,2:9]
oldcol=Coupon[,1:27]
newdata=cbind(oldcol,newcol)
head(newdata,5)
summary(Coupon$en_genre)
Category_= factor(Coupon$en_genre)
dummies2 = model.matrix(~Category_)
head(dummies2,3)
newcol2=dummies2[2:13]
oldcol2=newdata[,1:34]
NewCoupon=cbind(oldcol2,newcol2)
df$x <- paste(df$n,df$s)
#replace the number in columns of variable

#scatterplot
#correlation matrix
#correlation plot
#correlation test?
