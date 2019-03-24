#KNN


library(MASS)
library(caTools)
#Setting the working directory for the R-Script
setwd("D:\\Fall18 Classes\\IS777-DataAnalytics\\GroupC")

#Reading the CSV file as dataframe
df<-read.csv('Merged_Dataset_NO_ID_Factored.csv',sep='\t')


#Splitting Train and Test data
sample = sample.split(df,SplitRatio = 0.75) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train1 =subset(df,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test1=subset(df, sample==FALSE)


#Fitting KNN.reg model to the data
library(FNN)
#When K=3
pred_train<-knn.reg(train=train1, test=train1, y=train1$LOS, k = 3)
pred_test<-knn.reg(train=train1, test=test1, y=train1$LOS, k = 3)
#Error metrics for regression
library(DMwR)
#Train
cat("Error metrics on train data for k=3")
regr.eval(train1[,"LOS"], pred_train$pred)
cat("MAPE for k=3 on train data is",round(regr.eval(train1[,"LOS"], pred_train$pred)[4],4)*100,"%")

#Test
cat("Error metrics on test data for k=3")
regr.eval(test1[,"LOS"], pred_test$pred)
cat("MAPE for k=3 is",round(regr.eval(test1[,"LOS"], pred_test$pred)[4],4)*100,"%")


#Accuracay of the model is 45.13%





#When k=5
pred5_train<-knn.reg(train=train1, test=train1, y=train1$LOS, k = 5)
pred5_test<-knn.reg(train=train1, test=test1, y=train1$LOS, k = 5)
#Error metrics for regression

#Train
cat("Error metrics on train data for k=5")
regr.eval(train1[,"LOS"], pred5_train$pred)
cat("MAPE for k=5 on train data is",round(regr.eval(train1[,"LOS"], pred5_train$pred)[4],4)*100,"%")

#Test
cat("Error metrics on test data for k=5")
regr.eval(test1[,"LOS"], pred5_test$pred)
cat("MAPE for k=5 is",round(regr.eval(test1[,"LOS"], pred5_test$pred)[4],4)*100,"%")
#Accuracay is 47.33 %



#Further Increasing the K value 
#When k=10
pred10_train<-knn.reg(train=train1, test=train1, y=train1$LOS, k = 10)
pred10_test<-knn.reg(train=train1, test=test1, y=train1$LOS, k = 10)
#Error metrics for regression

#Train
cat("Error metrics on train data for k=10")
regr.eval(train1[,"LOS"], pred5_train$pred)
cat("MAPE for k=10 on train data is",round(regr.eval(train1[,"LOS"], pred5_train$pred)[4],4)*100,"%")

#Test
cat("Error metrics on test data for k=10")
regr.eval(test1[,"LOS"], pred5_test$pred)
cat("MAPE for k=10 is",round(regr.eval(test1[,"LOS"], pred5_test$pred)[4],4)*100,"%")

#Accuracay is 47.33%





#When k=100
pred100_train<-knn.reg(train=train1, test=train1, y=train1$LOS, k = 100)
pred100_test<-knn.reg(train=train1, test=test1, y=train1$LOS, k = 100)
#Error metrics for regression

#Train
cat("Error metrics on train data for k=100")
regr.eval(train1[,"LOS"], pred100_train$pred)
cat("MAPE for k=100 on train data is",round(regr.eval(train1[,"LOS"], pred100_train$pred)[4],4)*100,"%")

#Test
cat("Error metrics on test data for k=100")
regr.eval(test1[,"LOS"], pred100_test$pred)
cat("MAPE for k=100 is",round(regr.eval(test1[,"LOS"], pred100_test$pred)[4],4)*100,"%")
#Model accuracy now increased to  63.87 %


#When K-500:
pred500_train<-knn.reg(train=train1, test=train1, y=train1$LOS, k = 500)
pred500_test<-knn.reg(train=train1, test=test1, y=train1$LOS, k = 500)
#Error metrics for regression

#Train
cat("Error metrics on train data for k=500")
regr.eval(train1[,"LOS"], pred500_train$pred)
cat("MAPE for k=500 on train data is",round(regr.eval(train1[,"LOS"], pred500_train$pred)[4],4)*100,"%")

#Test
cat("Error metrics on test data for k=100")
regr.eval(test1[,"LOS"], pred500_test$pred)
cat("MAPE for k=500 is",round(regr.eval(test1[,"LOS"], pred500_test$pred)[4],4)*100,"%")
#Model accuracy now increased to  66.46 %



#When K-700:
pred700_train<-knn.reg(train=train1, test=train1, y=train1$LOS, k = 700)
pred700_test<-knn.reg(train=train1, test=test1, y=train1$LOS, k = 700)
#Error metrics for regression

#Train
cat("Error metrics on train data for k=700")
regr.eval(train1[,"LOS"], pred700_train$pred)
cat("MAPE for k=700 on train data is",round(regr.eval(train1[,"LOS"], pred700_train$pred)[4],4)*100,"%")

#Test
cat("Error metrics on test data for k=100")
regr.eval(test1[,"LOS"], pred700_test$pred)
cat("MAPE for k=700 is",round(regr.eval(test1[,"LOS"], pred700_test$pred)[4],4)*100,"%")
#Model accuracy now increased to  66.67 %


#AS it can be seen there is much increase in the acuuracy. We can stop increasing the K value and take K=500 as optimum K for KNN Reg model