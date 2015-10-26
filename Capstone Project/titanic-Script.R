setwd("C:/Users/usureshkumar/Documents/R-Software/Dive into EDA -Project files/Data-Science/")
getwd()
titanic.train <- read.csv("C:/Users/usureshkumar/Documents/R-Software/Dive into EDA -Project files/Data-Science/Data/train.csv")
titanic.test <- read.csv("C:/Users/usureshkumar/Documents/R-Software/Dive into EDA -Project files/Data-Science/Data/test.csv")
str(titanic.train)
str(titanic.test)
table(titanic.train$Survived)
prop.table(table(titanic.train$Survived))
plot(density(titanic.train$Age, na.rm=TRUE))
plot(density(titanic.train$Fare, na.rm=TRUE))
plot(prop.table(table(titanic.train$Ticket, titanic.train$Survived))[,1])
plot(prop.table(table(titanic.train$Ticket, titanic.train$Survived))[,2])
plot(prop.table(table(titanic.train$Age, titanic.train$Survived))[,2])
plot(prop.table(table(titanic.train$Pclass, titanic.train$Survived))[,2])
plot(prop.table(table(titanic.train$Sex, titanic.train$Survived))[,2])
plot(prop.table(table(titanic.train$Age, titanic.train$Survived))[,2])
plot(prop.table(table(titanic.train$SibSp, titanic.train$Survived))[,2])
plot(prop.table(table(titanic.train$Parch, titanic.train$Survived))[,2])
plot(prop.table(table(titanic.train$Ticket, titanic.train$Survived))[,2])
plot(prop.table(table(titanic.train$Fare, titanic.train$Survived))[,2])
plot(prop.table(table(titanic.train$Cabin, titanic.train$Survived))[,2])
plot(prop.table(table(titanic.train$Embarked, titanic.train$Survived))[,2])
require(ggplot2)

ggplot(aes(y=Cabin, x=Survived), data = titanic.train) +
  geom_point()

titanic.train$CabinCha <- substr(titanic.train$Cabin, 1,1)
titanic.test$CabinCha <- substr(titanic.test$Cabin, 1,1)
titanic.train$CabinNum <-gsub("[[:alpha:]]","",titanic.train$Cabin)
titanic.test$CabinNum <-gsub("[[:alpha:]]","",titanic.test$Cabin)

titanic.train$Name <- as.character(titanic.train$Name)
title <- sapply(titanic.train$Name, FUN=function(x) {strsplit(x, '[,.]')[[1]][2]})
names(title) <- NULL
titanic.train$Title <- title

titanic.test$Name <- as.character(titanic.test$Name)
title.test <- sapply(titanic.test$Name, FUN=function(x) {strsplit(x, '[,.]')[[1]][2]})
names(title.test) <- NULL
titanic.test$Title <- title.test

# all variables PassengerId + Survived + Pclass + Name + Sex + Age + SibSp + Parch + Ticket + Fare + Cabin + Embarked
library(rpart)
library(rpart.plot)
#with Ticket
#titanic.CART <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch +Ticket + Fare + Embarked, data = titanic.train, method = 'class')

titanic.CART <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, data = titanic.train, method = 'class')
prp(titanic.CART)
titanic.CART.Predict <- predict(titanic.CART, newdata = titanic.train, type = 'class')
#titanic.test.CART.Predict <- predict(titanic.CART, newdata = titanic.test, type = 'class')
require(rattle)
?fancyRpartPlot
fancyRpartPlot(titanic.CART)

table(titanic.train$Survived, titanic.CART.Predict)

##install.packages('ggplot2')
library(caret)
library(e1071)
fitControl <- trainControl(method = 'cv', number =10 )
cartGrid <- expand.grid(.cp=(1:50)*0.001)
train(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, data = titanic.train, method = 'rpart', trControl = fitControl, tuneGrid = cartGrid)
titanic.CART <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, data = titanic.train, method = 'class', control = rpart.control(cp=0.003))
titanic.CART.Predict <- predict(titanic.CART, newdata = titanic.train, type = 'class')
table(titanic.train$Survived, titanic.CART.Predict)

titanic.test.CART.Predict <- predict(titanic.CART, newdata = titanic.test, type = 'class')

titanic.submit<- data.frame(PassengerId=titanic.test$PassengerId, Survived=titanic.test.CART.Predict)
str(titanic.submit)
write.csv(titanic.submit, file='genderclassmodel.csv', row.names = FALSE)

##stlibrary(randomForest)
##titanic.train$Survived <- as.factor(titanic.train$Survived)
##titanic.RF <-randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = titanic.train, nodesize = 25, ntree = 200, na.action = na.pass)
##titanic.RF.Predict <- predict(titanic.RF, newdata = titanic.train)
##table(titanic.train$Survived, titanic.RF.Predict)

##titanic.train$Name <- as.character(titanic.train$Name)
##title <- sapply(titanic.train$Name, FUN=function(x) {strsplit(x, '[,.]')[[1]][2]})
##names(title) <- NULL
##titanic.train$Title <- title

##titanic.test$Name <- as.character(titanic.test$Name)
##title.test <- sapply(titanic.test$Name, FUN=function(x) {strsplit(x, '[,.]')[[1]][2]})
##names(title.test) <- NULL
##titanic.test$Title <- title.test
##titanic.test$Title[415]
##titanic.test$Title[415] <- NA
##titanic.test$Title
