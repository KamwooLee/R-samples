# Data Mining
# Author: Kamwoo Lee
# Last modified: 5/5/2016


####################################################
# Part 1a: Classification Accuracy on Zipcode Data #
####################################################


##### KNN ##### ----- ok!
require(ElemStatLearn)
require(caret)
data(zip.train)
zip = data.frame(zip.train)
zip$X1 <- factor(zip$X1)
st = apply(zip[,2:257], 2, sd) # standard deviations 
zip[,2:257] = zip[,2:257] / matrix(data=st, nrow=dim(zip)[1], ncol=length(st), byrow=T) # normalize
tc <- trainControl("cv",10)
parameters <- expand.grid(.k=9)
KNN.fit <- train(X1 ~., data=zip, method="knn", trControl=tc, tuneGrid=parameters)
KNN.fit$results$Accuracy

##### LDA ##### ----- ok!
require(ElemStatLearn)
require(caret)
require(MASS)
data(zip.train)
zip = data.frame(zip.train)
zip$X1 <- factor(zip$X1)
tc <- trainControl("cv",10)
LDA.fit <- train(X1 ~., data=zip, method="lda", trControl=tc)
LDA.fit$result$Accuracy

##### QDA ##### ----- ok!
require(ElemStatLearn)
require(caret)
require(MASS)
data(zip.train)
zip = data.frame(zip.train)
zip$X1 <- factor(zip$X1)
zip[,-1] <- apply(zip[,-1], 2, jitter) # Need to jitter the data to avoid exact multicolinearity
tc <- trainControl("cv",10)
QDA.fit <- train(X1 ~ ., data=zip, method="qda", trControl=tc)
QDA.fit$result$Accuracy

##### Logistic Regression ##### ----- ok!
require(ElemStatLearn)
require(caret)
require(nnet)
data(zip.train)
zip = data.frame(zip.train)
zip$X1 <- factor(zip$X1)
tc <- trainControl("cv",10)
parameters <- expand.grid(.decay=0.1)
LR.fit <- train(X1 ~ ., data=zip, method="multinom", trControl=tc, tuneGrid=parameters, MaxNWts = 3000)
LR.fit$result$Accuracy

##### Decision Tree ##### ---- ok!
require(ElemStatLearn)
require(caret)
data(zip.train)
zip = data.frame(zip.train)
zip$X1 <- factor(zip$X1)
tc <- trainControl("cv",10)
parameters <- expand.grid(.cp=0.001)
DT.fit <- train(X1 ~., data=zip, method="rpart",trControl=tc, tuneGrid=parameters)
DT.fit$result$Accuracy

##### Boosting Decision Trees ##### ----- ok!
require(ElemStatLearn)
require(adabag)
require(rpart)
data(zip.train)
zip = data.frame(zip.train)
zip$X1 <- factor(zip$X1)
BDT.fit <- boosting.cv(X1 ~ ., v = 10, data = zip, mfinal = 10)
1-BDT.fit$error

##### Random Forest ##### ----- ok!
library(ElemStatLearn)
require(caret)
require(randomForest)
data(zip.train)
zip = data.frame(zip.train)
zip$X1 <- factor(zip$X1)
tc <- trainControl("cv",10)
parameters <- expand.grid(.mtry=3)
RF.fit <- train(X1 ~., data=zip, method="rf",trControl=tc,tuneGrid=parameters)
RF.fit$result$Accuracy

##### Support Vector Machine ##### ----- ok!
library(ElemStatLearn)
require(caret)
require(kernlab)
data(zip.train)
zip = data.frame(zip.train)
zip$X1 <- factor(zip$X1)
tc <- trainControl("cv",10)
parameters <- expand.grid(.C=5)
SVM.fit <- train(X1 ~., data=zip, method="svmLinear",trControl=tc,tuneGrid=parameters)
SVM.fit$result$Accuracy

##### Neural Net ##### ----- ok!
library(ElemStatLearn)
require(caret)
require(nnet)
data(zip.train)
zip = data.frame(zip.train)
zip$X1 <- factor(zip$X1)
tc <- trainControl("cv",10)
parameters <- expand.grid(.size=15, .decay=5e-04)
NN.fit <- train(X1 ~., data=zip, method="nnet",trControl=tc,tuneGrid=parameters, MaxNWts=5000, maxit=200)
NN.fit$result$Accuracy





#################################################
# Part 1b: Classification Accuracy on Iris Data #
#################################################

##### KNN ##### ----- ok!
require(caret)
data(iris)
st = apply(iris[,1:4], 2, sd) # standard deviations 
iris[,1:4] = iris[,1:4] / matrix(data=st, nrow=dim(iris)[1], ncol=length(st), byrow=T) # normalize
tc <- trainControl("cv",10)
parameters <- expand.grid(.k=9)
KNN.fit <- train(Species ~., data=iris, method="knn", trControl=tc, tuneGrid=parameters)
KNN.fit$result$Accuracy

##### LDA ##### ----- ok!
#install.packages("MASS")
require(caret)
require(MASS)
data(iris)
tc <- trainControl("cv",10)
LDA.fit <- train(Species ~., data=iris, method="lda", trControl=tc)
LDA.fit$result$Accuracy

##### QDA ##### ----- ok!
require(caret)
require(MASS)
data(iris)
tc <- trainControl("cv",10)
QDA.fit <- train(Species ~., data=iris, method="qda", trControl=tc)
QDA.fit$result$Accuracy

##### Logistic Regression ##### ----- ok!
require(caret)
require(nnet)
data(iris)
tc <- trainControl("cv",10)
parameters <- expand.grid(.decay=0.1)
LR.fit <- train(Species ~., data=iris, method="multinom", trControl=tc,tuneGrid=parameters)
LR.fit$result$Accuracy

##### Decision Tree ##### ----- ok!
require(caret)
data(iris)
tc <- trainControl("cv",10)
parameters <- expand.grid(.cp=0.2)
DT.fit <- train(Species ~., data=iris, method="rpart",trControl=tc,tuneGrid=parameters)
DT.fit$result$Accuracy

##### Boosting Decision Trees ##### ----- ok!
require(adabag)
require(rpart)
data(iris)
BDT.fit <- boosting.cv(Species ~ ., v = 10, data = iris, mfinal = 10,
                            control = rpart.control(maxdepth = 1))
1-BDT.fit$error

##### Random Forest ##### ----- ok!
require(caret)
require(randomForest)
data(iris)
tc <- trainControl("cv",10)
parameters <- expand.grid(.mtry=3)
RF.fit <- train(Species ~., data=iris, method="rf",trControl=tc,tuneGrid=parameters)
RF.fit$result$Accuracy

##### Support Vector Machine ##### ----- ok!
require(caret)
require(kernlab)
data(iris)
tc <- trainControl("cv",10)
parameters <- expand.grid(.C=5)
SVM.fit <- train(Species ~., data=iris, method="svmLinear",trControl=tc,tuneGrid=parameters)
SVM.fit$result$Accuracy

##### Neural Net ##### ----- ok!
require(caret)
require(nnet)
data(iris)
tc <- trainControl("cv",10)
parameters <- expand.grid(.size=3, .decay=0.1)
NN.fit <- train(Species ~., data=iris, method="nnet",trControl=tc,tuneGrid=parameters)
NN.fit$result$Accuracy




################################################
# Part 2: Regression Accuracy on Prostate Data #
################################################

##### KNN ##### ----- ok!
require(ElemStatLearn)
require(caret)
data(prostate)
prostate <- subset(prostate, select=-train)
st = c(apply(prostate[,1:8], 2, sd),1) # standard deviations 
prostate = prostate / matrix(data=st, nrow=dim(prostate)[1], ncol=length(st), byrow=T) # normalize
tc <- trainControl("cv",10)
parameters <- expand.grid(.k=12)
KNN.fit <- train(lpsa ~., data=prostate, method="knn", trControl=tc, tuneGrid=parameters)
KNN.fit$results$RMSE^2

##### Linear Regression (full model) ##### ----- ok!
require(ElemStatLearn)
require(caret)
data(prostate)
prostate <- subset(prostate, select=-train)
tc <- trainControl("cv",10)
LR.fit <- train(lpsa ~., data=prostate, method="lm", trControl=tc)
LR.fit$results$RMSE^2

##### Stepwise Linear Regression ##### ----- ok!
require(ElemStatLearn)
require(caret)
require(MASS)
data(prostate)
prostate <- subset(prostate, select=-train)
tc <- trainControl("cv",10)
SLR.fit <- train(lpsa ~., data=prostate, method="lmStepAIC", trControl=tc)
SLR.fit$results$RMSE^2

##### Linear Regression with PCA ##### ----- ok!
require(ElemStatLearn)
require(caret)
require(pls)
data(prostate)
prostate <- subset(prostate, select=-train)
tc <- trainControl("cv",10)
parameters <- expand.grid(.ncomp=8)
LRPCA.fit <- train(lpsa ~., data=prostate, method="pcr", trControl=tc, tuneGrid=parameters)
LRPCA.fit$results$RMSE^2

##### Ridge Regression ##### ----- ok!
require(ElemStatLearn)
require(caret)
require(elasticnet)
data(prostate)
prostate <- subset(prostate, select=-train)
tc <- trainControl("cv",10)
parameters <- expand.grid(.lambda=0.1)
RR.fit <- train(lpsa ~., data=prostate, method="ridge", trControl=tc, tuneGrid=parameters)
RR.fit$results$RMSE^2

##### Model Averaging w/ OR ##### ----- ok!
require(ElemStatLearn)
require(BMA)
require(crossval)
data(prostate)
prostate <- subset(prostate, select=-train)
X <- prostate[,-9]
Y <- prostate[,9]
predfun.bma = function(train.x, train.y, test.x, test.y)
{
  fit = bicreg(train.x, train.y, OR=20)
  ynew = predict(fit, test.x)$mean
  out = mean((ynew - test.y)^2)
  return(out)
}
cv.out = crossval(predfun.bma, X, Y, K=10, B=1)
cv.out$stat


##### Model Averaging w/o OR ##### ----- ok!
require(ElemStatLearn)
require(BMA)
require(crossval)
data(prostate)
prostate <- subset(prostate, select=-train)
X <- prostate[,-9]
Y <- prostate[,9]
predfun.bma = function(train.x, train.y, test.x, test.y)
{
  fit = bicreg(train.x, train.y, OR=10000000)
  ynew = predict(fit, test.x)$mean
  out = mean((ynew - test.y)^2)
  return(out)
}
cv.out = crossval(predfun.bma, X, Y, K=10, B=1)
cv.out$stat

##### Decision Tree ##### ----- ok!
require(ElemStatLearn)
require(caret)
require(rpart)
data(prostate)
prostate <- subset(prostate, select=-train)
tc <- trainControl("cv",10)
parameters <- expand.grid(.cp=0.012)
DT.fit <- train(lpsa ~., data=prostate, method="rpart", trControl=tc, tuneGrid=parameters)
DT.fit$results$RMSE^2

##### Boosting Decision Trees ##### ----- ok!
require(ElemStatLearn)
require(caret)
require(bst)
require(plyr)
data(prostate)
prostate <- subset(prostate, select=-train)
tc <- trainControl("cv",10)
parameters <- expand.grid(mstop=100, maxdepth=1, nu=0.1)
BDT.fit <- train(lpsa ~., data=prostate, method="bstTree", trControl=tc, tuneGrid=parameters)
BDT.fit$results$RMSE^2

##### Random Forest ##### ----- ok!
require(ElemStatLearn)
require(caret)
require(randomForest)
data(prostate)
prostate <- subset(prostate, select=-train)
tc <- trainControl("cv",10)
parameters <- expand.grid(.mtry=5)
RF.fit <- train(lpsa ~., data=prostate, method="rf", trControl=tc, tuneGrid=parameters)
RF.fit$results$RMSE^2

##### Support Vector Machine ##### ----- ok!
require(ElemStatLearn)
require(caret)
require(kernlab)
data(prostate)
prostate <- subset(prostate, select=-train)
tc <- trainControl("cv",10)
parameters <- expand.grid(.C=7)
SVM.fit <- train(lpsa ~., data=prostate, method="svmLinear", trControl=tc, tuneGrid=parameters)
SVM.fit$results$RMSE^2

##### Neural Net ##### ----- ok!
require(ElemStatLearn)
require(caret)
require(neuralnet)
data(prostate)
prostate <- subset(prostate, select=-train)
tc <- trainControl("cv",10)
parameters <- expand.grid(.layer1=3,.layer2=0,.layer3=0)
NN <- train(lpsa ~., data=prostate, method="neuralnet", trControl=tc, tuneGrid=parameters)
NN.fit$results$RMSE^2



