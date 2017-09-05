# R-Coding-K-FoldCheck-FinalAssignment-
install.packages("titanic")
install.packages("Amelia")  #This package helps in visualising missing values
install.packages("randomForest")   
install.packages("DAAG")
install.packages("rpart.plot")
install.packages("ROCR")
install.packages("Rcpp")
install.packages("gmodels")
install.packages("Hmisc")
install.packages("pROC")
install.packages("ResourceSelection")
install.packages("car")
install.packages("caret")
install.packages("dplyr")
library(ResourceSelection)
library(ROCR)
library(car)
library(Hmisc)
library(caret)
library(gmodels)
library(pROC)
library(dplyr)
install.packages("InformationValue")
library(InformationValue)
library(titanic)
library(rpart.plot)
library(InformationValue)
library(rpart)
library(randomForest)
library("DAAG")

setwd("D:/MICA/AMMA/Data/Second") #This working directory is the folder where all the Titanic data is stored

trainingSetTitanic_b<-read.csv('train.csv')
nrow(trainingSetTitanic_b)
View(trainingSetTitanic_b)
testSetTitanicConstant <-read.csv('test-3.csv')
trainingSetTitanic_c <- read.csv('train.csv')
trainingSetTitanic_fresh<-trainingSetTitanic_b
set.seed(900) 
trainingSetTitanic_fresh$rand <- runif(nrow(trainingSetTitanic_fresh))
trainingSetTitanic_fresh_start <- trainingSetTitanic_fresh[trainingSetTitanic_fresh$rand <= 0.65,]
titanic_test_start <- trainingSetTitanic_fresh[trainingSetTitanic_fresh$rand > 0.65,]
CrossTable(trainingSetTitanic_fresh$Survived)

library(Rcpp)
library(Amelia)   #For visualising Missing Values
missmap(df.data, main = "Missing values vs observed")
trainingSetTitanic_fresh <- trainingSetTitanic_fresh[!apply(trainingSetTitanic_fresh[,c("Pclass", "Sex", "SibSp", "Parch", "Fare", "Age")], 1, anyNA),]
trainingSetTitanic_fresh_NA_allcols <- trainingSetTitanic_b[!apply(trainingSetTitanic_b[,c("Pclass", "Sex", "SibSp", "Parch", "Fare", "Age")], 1, anyNA),]
nrow(trainingSetTitanic_b)
mean_age = mean(trainingSetTitanic_b$Age)
#finding NA values and replacing
#Alternatively for replacing the NA values we could use 
#for i in (1:nrow(trainingSetTitanic_MeanNewnewSet))
#is.na(trainingSetTitanic)
#And then replace the values that return true by Substitution
trainingSetTitanic_MeanNewnewSet <- trainingSetTitanic_fresh_start
trainingSetTitanic_MeanNewnewSet2 <- trainingSetTitanic_fresh_start
trainingSetTitanic_MeanNewnewSet$Age[is.na(trainingSetTitanic_MeanNewnewSet$Age)] = mean(trainingSetTitanic_MeanNewnewSet$Age, na.rm = TRUE)
trainingSetTitanic_MeanNewnewSet2$Age[is.na(trainingSetTitanic_MeanNewnewSet2$Age)] = mean(trainingSetTitanic_MeanNewnewSet2$Age, na.rm = TRUE)

#New Model building
full.model.titanic.mean <- glm(formula = Survived ~ Pclass + Sex + SibSp + Parch + Fare + Age,
                               data=trainingSetTitanic_MeanNewnewSet, family = binomial) #family = binomial implies that the type of regression is logistic
summary(full.model.titanic.mean)
fit.train.mean <- lm(formula = Survived ~ Pclass + Sex + SibSp + Parch + Fare + Age,
                     data=trainingSetTitanic_MeanNewnewSet2) #family = binomial implies that the type of regression is logistic
summary(fit.train.mean)
#Also to remove insignificant variables
#vif - remove those variables which have high vif >5
vif(fit.train.mean) 
trainingSetTitanic_MeanNewnewSet$Parch<-NULL
full.model.titanic.mean <- glm(formula = Survived ~ Pclass + Sex + SibSp + Fare + Age,
                               data=trainingSetTitanic_MeanNewnewSet, family = binomial) #family = binomial implies that the type of regression is logistic
summary(full.model.titanic.mean)
trainingSetTitanic_MeanNewnewSet$Fare<-NULL
full.model.titanic.mean <- glm(formula = Survived ~ Pclass + Sex + SibSp + Age,
                               data=trainingSetTitanic_MeanNewnewSet, family = binomial) #family = binomial implies that the type of regression is logistic
summary(full.model.titanic.mean)
trainingSetTitanic_MeanNewnewSet$prob = predict(full.model.titanic.mean, type=c("response"))
trainingSetTitanic_MeanNewnewSet$Survived.pred = ifelse(trainingSetTitanic_MeanNewnewSet$prob>=.5,'pred_yes','pred_no')
table(trainingSetTitanic_MeanNewnewSet$Survived.pred,trainingSetTitanic_MeanNewnewSet$Survived)
nrow(titanic_test)
titanic_test2_mean_newSet <- titanic_test_start
titanic_test2_mean_newSet$Age[is.na(titanic_test2_mean_newSet$Age)] = mean(titanic_test2_mean_newSet$Age, na.rm = TRUE)
titanic_test2_mean_newSet$prob = predict(full.model.titanic.mean, newdata=titanic_test2_mean_newSet, type=c("response"))
titanic_test2_mean_newSet$Survived.pred = ifelse(titanic_test2_mean_newSet$prob>=.5,'pred_yes','pred_no')
table(titanic_test2_mean_newSet$Survived.pred,titanic_test2_mean_newSet$Survived)

# Checking for both Jack and Rose's survival
#For this we first load the dataset that has credentials of both i.e. Book1
df.jackrose <- read.csv('Book1.csv')
df.jackrose$prob = predict(full.model.titanic.mean, newdata=df.jackrose, type=c("response"))
df.jackrose$Survived.pred = ifelse(df.jackrose$prob>=.5,'pred_yes','pred_no')
head(df.jackrose)
summary(df.jackrose)

print("Jack dies")
print("Rose Lives")


KfoldCross <- function(dataset,formula,family,k)
{
  object <- glm(formula=formula, data=dataset, family = family)
  CVbinary(object, nfolds= k, print.details=TRUE)
}
MyFirstMeanSquareFunction <- function(dataset,formula,value)
{
  LM_Object <- lm(formula=formula, data=dataset, value=value)
  LM_Object_sum <-summary(LM_Object)
  MeanSquaredErrorValue <- mean(LM_Object_sum$residuals^2)
  print("The Mean squared error value is")
  print(MeanSquaredErrorValue)
}

KFoldCrossFunctionObject <- KfoldCross(trainingSetTitanic_MeanNewnewSet,Survived ~ Pclass + Sex + SibSp + Age,binomial,10)
MeanSquaredErrorValue_Train <-MyFirstMeanSquareFunction(trainingSetTitanic_MeanNewnewSet,Survived ~ Pclass + Sex + SibSp + Age)
Summary(MeanSquaredErrorValue_Train)
table(trainingSetTitanic_MeanNewnewSet$Survived,round(KFoldCrossFunctionObject$cvhat))
print("The probability of accuracy is ")
print(KFoldCrossFunctionObject$acc.cv)
KFoldCrossFunctionObject.test <- KfoldCross(titanic_test2_mean_newSet,Survived ~ Pclass + Sex + SibSp + Age,binomial,10)
newTestUsingMeanSquaredErrorValue <-MyFirstMeanSquareFunction(titanic_test2_mean_newSet,Survived ~ Pclass + Sex + SibSp + Age)
table(titanic_test2_mean_newSet$Survived,round(KFoldCrossFunctionObject.test$cvhat))
print("The final KFold Test Value is")
print(KFoldCrossFunctionObject.test$acc.cv)
