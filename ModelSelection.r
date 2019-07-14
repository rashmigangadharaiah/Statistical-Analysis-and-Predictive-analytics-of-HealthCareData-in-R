##Load Libraries##
library(leaps)
library(glmnet)
library(ISLR)
library(leaps)
library(splines)
library(gam)

##Forward and Backward Selection

#Load combined dataset created by merging all 4 provided fils. call it Merged Dataset (Merged_DS)
Merged_DS = read.csv(file="c:/users/trist/desktop/is777/Merged_Dataset_NO_ID_Factored.csv", header=TRUE, sep="\t");

#Check structure of dataset
str(Merged_DS)

#The regsubsets() function performs best subset selection by identifying the best model 
#where best is quantified using RSS.
regfit.full=regsubsets(LOS ~ ., Merged_DS)

#Check summary of full model, An asterisk (*) indicates that a given variable is included in model.
summary(regfit.full)
#By default, regsubsets() only reports results up to the best eight-variable model.

#Generate model using all 32 variables by setting nvmax =32.
regfit.full=regsubsets(LOS ~ .,data=Merged_DS ,nvmax =32)

#check summary of generated model.
reg.summary = summary(regfit.full)

#The summary() function also returns R2, RSS, adjusted R2, Cp, and BIC, will use to find best overall model.
names(reg.summary)

#Check R Square, we notice R Square increased from 22% approx with 1 variable to 46% with all variables.
reg.summary$rsq
reg.summary$adjr2

#Plotting RSS, adjusted R2, Cp, and BIC for all of the models at once will
#help us decide which model to select. type="l" tells R to connect the plotted points with lines.
par(mfrow =c(2,2))
plot(reg.summary$rss ,xlab=" Number of Variables ",ylab=" RSS",type="l")
plot(reg.summary$adjr2 ,xlab =" Number of Variables ",ylab=" Adjusted RSq",type="l")


#The which.max() function can be used to identify the location of the maximum point of a vector
which.max(reg.summary$adjr2)    #gives 21

#The points()command puts points on a plot that has already been created.
#plot a red dot to indicate the model with the largest adjusted R2 statistic.
points(21, reg.summary$adjr2[21], col ="red",cex =2, pch =20)

#similarly we can plot the Cp and BIC statistics, and indicate the
#models with the smallest statistic using which.min().
plot(reg.summary$cp ,xlab =" Number of Variables ",ylab="Cp",type='l')
reg.summary$cp
which.min(reg.summary$cp)       #gives 20
points(20, reg.summary$cp[20], col ="red",cex =2, pch =20)

reg.summary$bic
which.min(reg.summary$bic)      #gives 15
plot(reg.summary$bic ,xlab=" Number of Variables ",ylab=" BIC",type='l')
points(15, reg.summary$bic[15], col =" red",cex =2, pch =20)

#See generated graphs.

#The regsubsets() function has a built-in plot() command which can be used to display 
#the selected variables for the best model with a given number of predictors,
plot(regfit.full ,scale ="r2")
plot(regfit.full,scale ="adjr2")
plot(regfit.full ,scale ="Cp")
plot(regfit.full ,scale ="bic")


#Apply forward or backward selection using regsubsets() method. 
regfit.fwd=regsubsets(LOS ~ .,data= Merged_DS ,nvmax =32, method ="forward")
summary(regfit.fwd)
reg.summary.fwd = summary(regfit.fwd)

#Check R square and Adjusted R Square, cp, Bic  of forward selection method
reg.summary.fwd$rsq
reg.summary.fwd$adjr2
reg.summary.fwd$cp
reg.summary.fwd$bic


which.max(reg.summary.fwd$adjr2)
which.min(reg.summary.fwd$cp)
which.min(reg.summary.fwd$bic)

#Backward selection
regfit.bwd=regsubsets(LOS ~ .,data= Merged_DS ,nvmax =32, method ="backward")
summary(regfit.bwd)

reg.summary.bwd = summary(regfit.bwd)

#Check R square and Adjusted R Square of backward selection method
reg.summary.bwd$rsq
reg.summary.bwd$adjr2
reg.summary.bwd$cp
reg.summary.bwd$bic


which.max(reg.summary.bwd$adjr2)
which.min(reg.summary.bwd$cp)
which.min(reg.summary.bwd$bic)

#Create training and testing dataset
set.seed (1)
train=sample(c(TRUE ,FALSE), nrow(Merged_DS),rep=TRUE)
test =(!train)

#apply regsubsets() to the training set in order to perform best subset selection.
regfit.best=regsubsets(LOS ~ .,data=Merged_DS[train ,], nvmax =32)


#make a model matrix from the testdata. model.matrix() function is used for building an "X" matrix from data.
test.mat=model.matrix(LOS ~ .,data=Merged_DS[test ,])

#Now we run a loop, and for each size i, we extract the coefficients from regfit.best for the 
#best model of that size, multiply them into the appropriate columns of the test model matrix to
#form the predictions, and compute the test MSE.

val.errors =rep(NA ,32)
for(i in 1:32){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]= mean((Merged_DS$LOS[test]-pred)^2)
}

#check errors
val.errors

#We find that the best model is the one that contains 15 variables.
#because it has mininum error
which.min(val.errors)   #gives 15.

#Check the coffiecients
coef(regfit.best,15)


#Since we have no predict() method for regsubsets(), so write our own.
predict.regsubsets =function (object ,newdata ,id ,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi =coef(object,id=id)
  xvars =names(coefi)
  mat[,xvars ]%*%coefi
}



#Apply K cross validation, here we create a vector that allocates each observation 
#to one of k = 10 folds, and we create a matrix in which we will store the results.
k=10
set.seed(1)
folds=sample(1:k,nrow(Merged_DS),replace =TRUE)
cv.errors = matrix(NA ,k,32, dimnames =list(NULL,paste(1:32)))

#This will give us 10 by 32 matrix, of which the (i, j)th element corresponds
#to the test MSE for the ith cross-validation fold for the best j-variable model.
for(j in 1:k){
  best.fit =regsubsets(LOS ~ .,data=Merged_DS[folds !=j,],nvmax =32)
  for(i in 1:32) {
    pred=predict(best.fit,Merged_DS[folds ==j,], id=i)
    cv.errors[j,i]=mean((Merged_DS$LOS[folds ==j]-pred)^2)
  }
}

#use apply() function to average over the columns of this matrix in order to obtain a 
#vector for which the jth element is the crossvalidation error for the j-variable model.
mean.cv.errors =apply(cv.errors ,2, mean)


#See mean error of cross validation
mean.cv.errors

par(mfrow =c(1,1))

#Plot error, type = 'b' indicate line.
plot(mean.cv.errors,type='b')

#We now perform best subset selection on the full data set in order to obtain the 11-variablen model.
reg.best=regsubsets(LOS ~ .,data=Merged_DS, nvmax =32)

#See the cofficient of 11 variable use for our best model selected by k fold cross validation.
coef(reg.best ,11)

# Use Generalized Additive Models in an exploratory fashion by relaxing the assumption of linearity for your numeric variables.
# You can use any splines technique to relax the assumption of linearity in your generalized additive models.
# After some experimentation, you should decide on how much non-linearity should be used for each variable.
# This decision should be based on predictive performance results.
# Present your final model along with the plots showing the relationship between x's and f(x)'s. 

# There should be a code that reads data and merges them and makes factor categorical data.

datafile = read.csv(file="c:/users/trist/desktop/is777/Merged_Dataset_NO_ID_Factored.csv", header=TRUE, sep="\t")



gam1 = gam(LOS~ns(age,11) + ns(income, 1) + ns(mortscore, 4), data=datafile)
gam2 = gam(LOS~ns(age,11) + ns(income, 1) + ns(mortscore, 5), data=datafile)
gam3 = gam(LOS~ns(age,1) + ns(income, 1) + ns(mortscore, 4), data=datafile)
gam4 = gam(LOS~ns(age,1) + ns(income, 1) + ns(mortscore, 5), data=datafile)

par(mfrow=c(1,3))
plot(gam1, se=TRUE,col="blue")
plot(gam2, se=TRUE,col="blue")
plot(gam3, se=TRUE,col="blue")
plot(gam4, se=TRUE,col="blue")

summary(gam1)
summary(gam2)
summary(gam3)
summary(gam4)

anova(gam1,gam2,gam3,gam4,test="F")

##Ridge Regression ##

v1<-read.csv("c:/users/trist/desktop/is777/Merged_Dataset_NO_ID_Factored.csv",sep='\t')
head(v1)
sum(is.na(v1$LOS))
v1=na.omit(v1)


#Fitting ridge regression model
x=model.matrix(v1$LOS~.,v1)[,-1]
y=v1$LOS
grid=10^seq(10,-2,length =100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod ))
#value of coefficients when lambda is large
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[ -1 ,50]^2))
#value of coefficients when lambda is small
ridge.mod$lambda [60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[ -1 ,60]^2))
#Splitting model into test set and training set, to estimate the test error of ridge regression
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]
#Fitting regression model on training set and evaluate its MSE on test set
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
#Fitting regression model on training set and evaluate its MSE on test set, lambda=10^10
ridge.pred=predict(ridge.mod ,s=1e10 ,newx=x[test,])
mean(( ridge.pred -y.test)^2)
#cross-validation to choose the tuning parameter ??.
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
#refit ridge regression on full dataset using lambda value obtained by using cv
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam )[1:33,]

## Lasso ##
#Reading the CSV file as dataframe
v1<-read.csv('c:/users/trist/desktop/is777/Merged_Dataset_NO_ID_Factored.csv',sep='\t')

head(v1)
summary(v1)

names(v1)
dim(v1)

#checking for missing elements in the response variable if any:#there is none

sum(is.na(v1$LOS))

regfit.full=regsubsets(LOS~.,data=v1 ,nvmax =32)
reg.summary =summary (regfit.full)
names(reg.summary )


reg.summary$rsq


x=model.matrix (LOS~.,v1 )[,-1]
y=v1$LOS


set.seed (1)
train=sample (1: nrow(x), nrow(x)/2)
test=(- train )
y.test=y[test]



grid =10^ seq (10,-2, length =100)
lasso.mod =glmnet (x[train ,],y[train],alpha =1, lambda =grid)
plot(lasso.mod)
#We can see from the coefficient plot that depending on the choice of tuning
#parameter, some of the coefficients will be exactly equal to zero.




#Let us perform  cross validation and compute the associated error
set.seed (1)
cv.out =cv.glmnet (x[train ,],y[train],alpha =1,lambda=grid)
plot(cv.out)
bestlam =cv.out$lambda.min
lasso.pred=predict (lasso.mod ,s=bestlam ,newx=x[test ,])
mean(( lasso.pred -y.test)^2)


#MSE is 51.32. which is substantially lower than the null model and of least square 

out=glmnet (x,y,alpha =1, lambda =grid)

lasso.coef=predict (out ,type ="coefficients",s=bestlam )[1:32 ,]
lasso.coef
lasso.coef[lasso.coef !=0]

#there are 7 variables co-eeficients which are exactly zero.



