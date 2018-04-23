titanic<-read.csv("titanic3.csv",header=TRUE)
set.seed(1)
idx<-seq_len(nrow(titanic))
sampTraining<-sample(idx, 1047)
training<-titanic[sampTraining,]
numTest=0
for (i in 1:1309)
{
	if (i %in% sampTraining) {}
	else{
		if (numTest<1) {sampTest<-c(i)}
		else{sampTest<-c(sampTest[1:numTest],i)}
		numTest=numTest+1
		}
}
test<-titanic[sampTest,]

print("Training NA sum:")
badTraining<-is.na(training)
colSums(badTraining)
print("Training NA sum:")
badTest<-is.na(test)
colSums(badTest)

training$sex=as.numeric(training$sex)
training$cabin=as.numeric(training$cabin)
training$embarked=as.numeric(training$embarked)
temp<-training[,c(1,2,4,5,6,7,11)]
training1<-na.omit(temp)
cor(training1)
glm.out = glm(survived ~ pclass*sex*age*sibsp*parch*embarked, family=binomial(logit), data=training1)
summary(glm.out)

test$sex=as.numeric(test$sex)
test$cabin=as.numeric(test$cabin)
test$embarked=as.numeric(test$embarked)
temp<-test[,c(1,2,4,5,6,7,11)]
test1<-na.omit(temp)

pred<-predict(glm.out,test1[,-2])
table(round(pred),test1[,2])

install.packages("e1071")
library(e1071)

pre <- prediction(pred,test1[,2])
performance(pre,'auc')@y.values
perf <- performance(pre,'tpr','fpr')
plot(perf)


x<-training1[,-2]
y<-training1[,2]
tune.svm(x,y,gamma=2^(-1:1),cost=2^(2:4),kernel=“linear”)
tune.svm(x,y,gamma=2^(-1:1),cost=2^(2:4),kernel=“radial”)
svm_linear <- svm(x,y,gamma=0.5,cost=4,kernel="linear")
svm_radial <- svm(x,y,gamma=0.5,cost=4,kernel="radial")

pred2 <- predict(svm_radial,test1[,-2])
table(round(pred2),test1[,2])

pre2 <- prediction(pred2,test1[,2])
performance(pre2,'auc')@y.values
perf2 <- performance(pre2,'tpr','fpr')
plot(perf2)
