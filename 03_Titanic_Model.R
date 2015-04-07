# load required packages
library(dplyr)
library(cluster)
library(psych)
library(randomForest)
library(rpart)
library(Hmisc)
library(party)

#Richard Shanahan  
#https://github.com/rjshanahan  
#6 April 2015

#INFS 5098: PROJECT: R code to cleanse Kaggle Titanic data set inc. feature engineering

############ 1. run prediction model using 'rpart' poackage ###############

##USE RPART
#remove cap on partitions
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + 
               Embarked + title + familysize + nickname + altname + iceberg + deck +
               subclass + faregroup + classregion, 
             data=titanic_train, 
             method="class",
             control=rpart.control(minsplit=2, cp=0))
fit

#trim trees options
new.fit <- prp(fit,snip=TRUE)$obj

#pretty randomforest plot
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)             #generate plot

############ 2. run prediction model using 'randomForest' poackage ###############

#maintain this for reproducibility
set.seed(499)

#submission 3 0.76555 with recoded title
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + 
                        Embarked + title + familysize + nickname + altname + iceberg + deck +
                        subclass + faregroup + classregion + 
                        farepp + marriagelength + childage + side, 
                      data=titanic_train, 
                      importance=TRUE, 
                      ntree=1500)

#plot results to determine variable importance
varImpPlot(fit)

#ACCURACY: how worse the model performs without each variable, so a high decrease in accuracy would be expected for very predictive variables
#GINI: measures how pure the nodes are at the end of the tree. Again it tests to see the result if each variable is taken out and a high score means the variable was important.

############ 3. run prediction model using 'ctree' package ###############
library(party)

fit <- ctree(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + 
               Embarked + title + familysize + nickname + altname + iceberg + deck +
               subclass + faregroup + classregion, 
             data=titanic_train)

plot(fit)

############ 99. run prediction model and generate output ###############

# model execution
Prediction <- predict(fit, titanic_test)                      #for rpart and randomforest
Prediction <- predict(fit, titanic_test, type="response")     #for ctree

# create dataframe and submission file for Kaggle
submit <- data.frame(PassengerId = titanic_test$PassengerId, Survived = as.vector(Prediction))
write.csv(submit, file = "kagglesubmission_05.csv", row.names = FALSE)

