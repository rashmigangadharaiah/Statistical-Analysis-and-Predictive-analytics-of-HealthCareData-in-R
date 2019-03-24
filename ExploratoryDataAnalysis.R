'''Cdemoses = read.csv(file="Cdemoses.csv", header=TRUE, sep="\t")
Chealth = read.csv(file="Chealth.csv", header=TRUE, sep="\t")
Cquality = read.csv(file="Cquality.csv", header=TRUE, sep="\t")
Cutilization = read.csv(file="Cutilization.csv", header=TRUE, sep="\t")
str(Cdemoses)
str(Chealth)
str(Cquality)
str(Cutilization)

Merged_Cdemoses_Chealth <- merge(Cdemoses,Chealth,by="id", all.x=TRUE)
Merged_CQuality <- merge(Merged_Cdemoses_Chealth,Cquality,by="id", all.x=TRUE)
Merged_Dataset <- merge(Merged_CQuality,Cutilization,by="id", all.x=TRUE)
write.table(Merged_Dataset, file="Merged_Dataset.csv", sep="\t", row.names=FALSE, quote=FALSE)
'''

setwd("D:\\Fall18 Classes\\IS777-DataAnalytics\\GroupC")
df<-read.csv('Merged_Dataset_NO_ID_Factored.csv',sep='\t')
head(df1)
summary(df)
max(df)
#df1<-subset(df,select=c("age","income","mortscore","LOS"))
#str(df1)

#df1$mortscore<-as.numeric(df1$mortscore)
#df1[ ,1:3] <- scale(df1[ ,1:3]) 
#str(df1)
#head(df1)

#applying the full data on model to check for the predictors with less p-value and high co-efficients

linearmod<-lm(LOS~age+income+mortscore+betterbed+betterwalk+betterbath+bettermove+bettertaking+betterheal+bettermove+drugu+alcolu+istimely+taughtdrug+checkfall+checkdepression+checkflushot+checkvaccine+checkfootcare+HHTcare+HHTcomm+HHTdiscuss+admittedhospital+urgent+readmittedhospital+emergencyhospital,data=df)
summary(linearmod)
coef(linearmod)

#After analysing the summary, we now the below features are hilghy corelated with LOS
#hence consider the below features for our modelling
linearmod<-lm(LOS~age+income+betterbed+drugu+alcolu+istimely+checkfall+checkdepression+checkvaccine+checkfootcare+admittedhospital+emergencyhospital,data=df)
summary(linearmod)
coef(linearmod)


#Splitting Train and Test data
sample = sample.split(df,SplitRatio = 0.75) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train1 =subset(df,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test1=subset(df, sample==FALSE)


#when considering the all the important features.Assuming abolute values of coefficient. 
linearmodall<-lm(LOS~age+linearmodall<-lm(LOS~age+mortscore+betterbed+drugu+alcolu+checkfall+checkdepression+checkvaccine+checkfootcare+admittedhospital+emergencyhospital,data=df)betterbed+drugu+alcolu+istimely+checkfall+checkdepression+checkvaccine+checkfootcare+admittedhospital+emergencyhospital,data=df)
distPred <- predict(linearmodall, test1)
summary (linearmodall)
actuals_predsall <- data.frame(cbind(actuals=test1$LOS, predicteds=distPred))
correlation_accuracy <- cor(actuals_predsall)
min_max_accuracyall <- mean(apply(actuals_predsall, 1, min) / apply(actuals_predsall, 1, max)) 
#The model results is 77.56% accuray with the combination of features. the multimple linear model with the features selection increses the accuracy.
mape <- mean(abs((actuals_predsall$predicteds - actuals_predsall$actuals))/actuals_predsall$actuals)

coef(linearmodall)
plot(linearmodall)
return()

plot(predict(linearmodall),df,
     xlab="predicted",ylab="actual")
abline(a=0,b=1)
par(mfrow = c(2, 2))
plot(linearmodall)

# when considering only the top variables with positive values
linearmod<-lm(LOS~alcolu+checkfall+checkdepression+checkvaccine+checkfootcare+admittedhospital+emergencyhospital,data=df)
distPred <- predict(linearmod, test1)
summary (linearmod)
actuals_preds <- data.frame(cbind(actuals=test1$LOS, predicteds=distPred))
correlation_accuracy <- cor(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) 
#this model is doing good. the accuracy is 74.7% compared to previous model this model is predicting the response values almost near to the actual values
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)







#Splitting Train and Test data
sample = sample.split(df1,SplitRatio = 0.75) # splits the data in the ra
train1 with rows which are marked as TRUE
test1=subset(df1, sample==FALSE)

#linear regression:
linearMod <- lm(age ~ LOS, data=train1)
distPred <- predict(linearMod, test1)
summary (linearMod)
actuals_preds <- data.frame(cbind(actuals=test1$LOS, predicteds=distPred))
correlation_accuracy <- cor(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) 
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)

#multiple linearq regression:
