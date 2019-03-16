train<-read.csv(file.choose())
test<-read.csv(file.choose())

#How many percentage of total pessengers survive?
prop.table(table(train$Sex,train$Survived),1) 
# Female has higher chance to survive from disaster
prop.table(table(train$Embarked,train$Survived),1) 
#class c has higher chance to survive from disaster compared with other classes.

train$Child<-0
train$Child[train$Age<18]<-1

aggregate(Survived~Child+Sex, data=train, FUN=sum)
aggregate(Survived~Child+Sex, data=train, FUN=function(x){sum(x)/length(x)})
#Female Child has higher chance to survive from disaster


#Data preparation
trainset<-subset(train,select=c('Survived', 'Pclass', 'Sex', 'Age', 'SibSp', 'Parch', 'Embarked','Fare'))

testset<-subset(test,select=c('Pclass', 'Sex', 'Age', 'SibSp', 'Parch', 'Embarked','Fare'))

#remove null value in 'Embarked' 
trainset<-subset(trainset,Embarked!="")
trainset$Embarked = droplevels(trainset$Embarked, "")
testset<-subset(testset,Embarked!="")

#set null value in 'age' to median age 
trainset$Age[is.na(trainset$Age)]<-median(trainset$Age,na.rm=TRUE)
testset$Age[is.na(testset$Age)]<-median(testset$Age,na.rm=TRUE)

#convert sex into binary variable
trainset$Sex<-ifelse(trainset$Sex=="male",1,0)
testset$Sex<-ifelse(testset$Sex=="male",1,0)

#Create Title 
trainset$Name<-as.character(trainset$Name)
strsplit(trainset$Name,split='[,.]')[[1]]
trainset$Title<-sapply(trainset$Name,FUN=function(x){strsplit(x,split='[,.]')[[1]][2]})

table(trainset$Title)

trainset$Title[trainset$Title %in% c('Mlle','Mme')]<-"Mlle"
trainset$Title[trainset$Title %in% c('Dona','Jonkheer','the Countess','Lady')]<-"Lady"
trainset$Title[trainset$Title %in% c('Capt','Don','Major','Sir')]<-"Sir"
trainset$Title<-factor(trainset$Title)

table(trainset$Title)

#create a family size
trainset$familysize<-as.numeric(trainset$SibSp+trainset$Parch+1)
testset$familysize<-testset$SibSp+testset$Parch+1

#Match Data, removing noise
library(MatchIt)
trainset_match<-matchit(Survived~Pclass+Sex+Age+SibSp+Parch+Embarked+Fare+familysize,method="nearest",data=trainset)
trainsetmatch<-match.data(trainset_match)


#Logistic Regression_prediction1
trainlog<-glm(Survived~Pclass+Sex+Age+SibSp+Parch+Embarked+Fare+familysize,family=binomial(),data=trainsetmatch)
summary(trainlog) 

#remove insignificant variables
trainlog_reduced<-glm(Survived~Pclass+Sex+Age,family=binomial(),data=trainsetmatch)
summary(trainlog_reduced)

testset_predict1<-subset(testset,select=c('Pclass', 'Sex', 'Age'))

testset_predict$survivedornot<-predict(trainlog_reduced,newdata = testset_predict,type = "response")

testset_predict$survivedornot<-ifelse(testset_predict$survivedornot>0.5,1,0)

testset_predict_final<-cbind(test,testset_predict)
survivedornot_prediction<-subset(testset_predict_final, select = c("PassengerId","survivedornot"))


#Decision Tree_prediction2
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fit<-rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Embarked+Fare+familysize, data=trainsetmatch, method="class")
plot(fit)
text(fit)

fancyRpartPlot(fit)

testset_predict2<-subset(testset,select=c('Pclass', 'Sex', 'Age',"SibSp","Parch","Embarked","Fare","familysize"))

testset_prediction2<-predict(fit,newdata = testset_predict2,type = "class")
submit<-data.frame(PassengerId=test$PassengerId,Survived=testset_prediction1)
write.csv(submit,file="deciciontree_prediction")

#random forest_prediction3

library(party)
randomf_fit<-cforest(as.factor(Survived)~Pclass+Sex+Age+SibSp+Parch+Embarked+Fare+familysize, data=trainset,controls=cforest_unbiased(ntree=2000,mtry=3))

testset_prediction3<-predict(randomf_fit,newdata = testset,OOB=TRUE,type = "response")
submit3<-data.frame(PassengerId=test$PassengerId,Survived=testset_prediction3)
write.csv(submit3,file="randomf_prediction")
