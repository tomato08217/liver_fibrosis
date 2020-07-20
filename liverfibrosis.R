setwd("D:/srrs/liver fibrosis/old/paper")
data<-read.csv("")
indexes<-sample(108,0.7*108,replace=FALSE) 
train<-data[indexes,]
test<-data[-indexes,]

#model
fit<-glm(CustomLabel~+++],family = "binomial",data=train)
library(leaps)
leaps<-regsubsets(CustomLabel~original_glcm_Autocorrelation+waveletHHH_glrlm_LongRunLowGrayLevelEmphasis+waveletHLL_glrlm_RunPercentage+waveletLLL_gldm_DependenceVariance+original_firstorder_Mean+square_glcm_DifferenceVariance,data=train)
plot(leaps)

#prediction
pred.train <- predict(fit, train, type="response")
pred.test <- predict(fit, test, type="response")
#ROC
library(pROC)
roc1<-roc(train$CustomLabel,pred.train,ci=TRUE, of="auc")
roc1$ci #CI
plot(roc1, print.auc=TRUE,print.thres=TRUE,col="blue")
roc2<-roc(test$CustomLabel,pred.test,ci=TRUE)
roc2$ci #CI
plot(roc2, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE)
#叠加
plot(roc2,print.auc=TRUE,print.thres=TRUE,col="red") #叠加

roc.test(roc1, roc2, reuse.auc=FALSE)
summary(fit)#??????????????????
exp(coef(fit))#??????OR
exp(confint(fit))#??????95%CI OR
confint(fit)#??????95%Coef

#confusion matrix

predicted.classes.train <- ifelse(pred.train > 0.5, "1", "0")
predicted.classes.test <- ifelse(pred.test > 0.307, "1", "0")

#画confusionMatrix
library(caret)
confusionMatrix(table(train$CustomLabel, predicted.classes.train)) 
confusionMatrix(table(test$CustomLabel, predicted.classes.test)) 
