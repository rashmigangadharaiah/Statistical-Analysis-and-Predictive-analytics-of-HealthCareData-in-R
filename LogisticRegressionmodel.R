v1<-read.csv("E:\\Fall 2018\\IS777\\D3\\Standardized_data.csv")
head(v1)
v2<-subset(v1, select=c("age","income","mortscore"))
#To produce matrix of scatterplot
pairs(v2)
#Correlation matrix
cor(v2)
#Converting class label to binary for Logistic Regression Purpose
v3<-subset(v1,select="LOS")
head(v3)
#Converting class of object v3 to numeric
v4<-as.numeric(unlist(v3))
class(v4)
#Cut function is used to bin the class label into binary values
cut(v4,2, labels=c("Less","More"))
#Replace existing column in v1 with the binary values
v1$LOS<-cut(v4,2, labels=c("Less","More"))
v1
#Selecting predictors for logistic regression
v6<-subset(v1,select=c("age","income","mortscore","LOS"))
head(v6)
#Fit Logistic Regression model in order to predict LOS
glm.fits=glm(LOS~income+mortscore+age,data=v6,family=binomial)
summary(glm.fits)
coef(glm.fits)
#Predict function used to predict LOS will be less or more
glm.probs =predict (glm.fits,type ="response")
#Printing first 10 probabilities
glm.probs[1:10]
#Creates dummy variables
contrasts(v6$LOS)
glm.pred=rep("Less",99778)
glm.pred[glm.probs>.5]="More"
#Confusion matrix for LOS
table(glm.pred,v6$LOS)
(83162+1671)/99778
mean(glm.pred==v6$LOS)

