#1

# First, get the sampling training data and test data 
setwd("~/Desktop")
titanic <- read.csv("titanic3.csv",header=TRUE)
set.seed(1)
index <- sample(1:nrow(titanic),nrow(titanic)*0.8)
training <- titanic[sort(index),]
test <- titanic[-index,]
training$sex=as.numeric(training$sex)
training$embarked=as.numeric(training$embarked)
training$ticket=as.numeric(training$ticket)
test$sex = as.numeric(test$sex) 
test$embarked=as.numeric(test$embarked)
test$ticket=as.numeric(test$ticket)

#Second, omit the null data
index_withAge <- array(1:830,1)
index_withoutAge <- array(1:217,1)
numWithoutAge=0;
numWithAge=0;
for (i in 1:1047)
{
	if (is.na(training$age[i])==FALSE) 
	  {
	  	if (numWithAge<1){index_withAge<-c(i)}
	 	else {index_withAge<-c(index_withAge[1:numWithAge],i)}
	  	numWithAge = numWithAge+1;
	  }
}
training_withAge=training[index_withAge,]
numWithAge=0
for (i in 1:262 )
    if (is.na(test$age[i])==FALSE)
    {
    	if (numWithAge<1) {index_withAge<-c(i)}
    	else {index_withAge <- c(index_withAge[1:numWithAge],i)}
    	numWithAge = numWithAge+1
    }
test_withAge = test[index_withAge,]
training = training_withAge[-779, ]        # 779th data has null fare attribute
test = test_withAge

#2
library(tree)
training$survived = as.factor(training$survived) # the following tree() requires its type to be factor
training.ltr <- tree(survived ~ pclass + sex + age + sibsp + parch + ticket + embarked + fare, training)
training.ltr
summary(training.ltr)
plot(training.ltr)
text(training.ltr)

#4 
training.cv <- cv.tree(training.ltr,, prune.misclass)
# prune.misclass is an abbreviation for prune.tree(method="misclass")
plot(training.cv)

#5
training.ltr.best <- prune.tree(training.ltr,,6)
pred <-  predict(training.ltr.best,test[,-2], type="class")
#without type="class", it will apply regression model instead of classification method(decision tree), which result in decimal number wrongly.
pred <- as.numeric(pred) 		 #round() does not apply to type "factor"
table(round(pred),test[,2])

library(ROCR)
pre <- prediction(pred,test[,2])
performance(pre,'auc')@y.values 
perf <- performance(pre,'tpr','fpr') plot(perf) 

#6
install.packages("randomForest")
library(randomForest)
set.seed(1)

training.rf<-randomForest(survived ~ pclass + sex + age + sibsp + parch + ticket + embarked + fare,data=training, ntree=100)
pred <-  predict(training.rf,test[,-2], type="class")
pred <- as.numeric(pred)
table(round(pred),test[,2])
pre <- prediction(pred,test[,2])
performance(pre,'auc')@y.values
perf <- performance(pre,'tpr','fpr') plot(perf) 

#7
max_auc=0
bestNumber=0
for (i in 1:600)
{
	set.seed(1)
   training.rf<-randomForest(survived ~ pclass + sex + age + sibsp + parch + ticket + embarked + fare,data=training, ntree=i)
   pred <-  predict(training.rf,test[,-2], type="class")
   pred <- as.numeric(pred)
  pre <- prediction(pred,test[,2])
  auc <- performance(pre,'auc')@y.values
  auc<-as.numeric(auc)
  if (auc > max_auc) {max_auc=auc; bestNumber=i}
  }
  training.rf.best<-randomForest(survived ~ pclass + sex + age + sibsp + parch + ticket + embarked + fare,data=training, ntree=53)
pred <-  predict(training.rf,test[,-2], type="class")
pred <- as.numeric(pred)
pre <- prediction(pred,test[,2])
performance(pre,'auc')@y.values
perf <- performance(pre,'tpr','fpr') plot(perf) 
  