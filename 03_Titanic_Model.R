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
#28 April 2015

#INFS 5098: PROJECT: R code for various prediction models

############ 1. run prediction model using 'rpart' package ###############

##USE RPART
#remove cap on partitions
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + 
               Embarked + title + familysize + nickname + altname + iceberg + deck +
               subclass + faregroup + classregion + childage + marriagelength, 
             data=droplevels(titanic_train), 
             method="class",
             control=rpart.control(minsplit=2, cp=0))
fit

#trim trees options
#new.fit <- prp(fit,snip=TRUE)$obj

#pretty randomforest plot - expensive to run
#library(rattle)
#library(rpart.plot)
#library(RColorBrewer)

#fancyRpartPlot(fit)             #generate plot

# model execution
Prediction <- predict(fit, titanic_test)                      #for rpart and randomforest

# create dataframe and submission file for Kaggle
submit <- data.frame(PassengerId = titanic_test$PassengerId, Survived = as.vector(Prediction))
write.csv(submit, file = "kagglesubmission_rpart.csv", row.names = FALSE)

############ 2. run prediction model using 'randomForest' package ###############

#maintain this for reproducibility
set.seed(499)

#submission 3 0.76555 with recoded title
fit <- randomForest(droplevels(as.factor(Survived)) ~ Pclass + Sex + Age + SibSp + Parch + Fare + 
                        Embarked + title + familysize + nickname + altname + iceberg + deck +
                        subclass + faregroup + classregion + 
                        farepp + marriagelength + childage + side, 
                      data=titanic_train, 
                      importance=TRUE, 
                      ntree=2000)

#plot results to determine variable importance
varImpPlot(fit)

#ACCURACY: how worse the model performs without each variable, so a high decrease in accuracy would be expected for very predictive variables
#GINI: measures how pure the nodes are at the end of the tree. Again it tests to see the result if each variable is taken out and a high score means the variable was important.

#code to generate prettier variable importance plot
varImpdf <- data.frame(var = factor(names(fit$variable.importance)),
                       imp = fit$variable.importance,
                       row.names=NULL)

ggplot(data = varImpdf,
       aes(x=reorder(varImpdf$var, -varImpdf$imp),
           y=imp,
           fill=var)) +
  #scale_x_discrete(limits = varorder) +
  xlab("Titanic Attribute - note: lowercase vars are produced through feature selection/engineering") + 
  ylab("Variable Importance") + 
  geom_bar(stat="identity") +
  ggtitle("Kaggle Titanic: Variable Importance Ratings using 'randomForest'")

# model execution
Prediction <- predict(fit, titanic_test)                      #for rpart and randomforest

# create dataframe and submission file for Kaggle
submit <- data.frame(PassengerId = titanic_test$PassengerId, Survived = as.vector(Prediction))
write.csv(submit, file = "kagglesubmission_randomforest.csv", row.names = FALSE)

############ 3. run prediction model using 'ctree' package ###############
library(party)

fit <- ctree(droplevels(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + 
               Embarked + titlegroup + familysize + nickname + altname + iceberg + deck +
               subclass + faregroup + classregion + childage + marriagelength, 
             data=titanic_train,
             controls=cforest_unbiased(ntree=4000, mtry=2))
# 
# fit <- ctree(droplevels(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + 
#                Embarked + titlegroup + familysize + deck + side , 
#              data=titanic_train,
#              controls=cforest_unbiased(ntree=4000, mtry=2))

plot(fit)

# model execution
Prediction <- predict(fit, titanic_test, OOB=T, type="response")     #for ctree

# create dataframe and submission file for Kaggle
submit <- data.frame(PassengerId = titanic_test$PassengerId, Survived = as.vector(Prediction))
write.csv(submit, file = "kagglesubmission_ctree.csv", row.names = FALSE)


############ 4. LOGIT model and generate output ###############

# create variable vectors for inclusion in model
var_exist <- c(
  'Pclass',        
  'Sex',           
  'Age',           
  'SibSp',
  'Fare',
  'Parch',         
  'Fare',          
  'Embarked')

var_new <- c(
  'nickname',   
  'titlegroup',
  'altname',       
  'side',          
  'deck',    
  'farepp',        
  'marriagelength',
  'childage',      
  'faregroup')  

var_independent <- c(var_new, var_exist)

var_dependent <- 'Survived'

# define logistic regression formula
logit_formula <- paste(var_dependent, paste(var_independent, collapse='+'), sep = '~')

# run model against training dataset to determine coefficients
model <- glm(logit_formula, 
             data=titanic_train, 
             family=binomial(link='logit'))

# view coefficients
as.integer(coefficients(model))

# assess significance of coefficients
summary(model)

# run prediction using model
titanic_train$pred <- predict(model,
                             newdata=titanic_train,
                             type="response")

# compare predictions against actual in TRAIN
ggplot(titanic_train, 
       aes(x=pred, 
           color=Survived, 
           linetype=Survived)) +
  geom_density() +
  ggtitle("Titanic Training Dataset_LOGIT Predictions") 


# generate predictions for TEST dataset
titanic_test$pred <- predict(model,
                              newdata=titanic_test,
                              type="response")

# convert probabilities to 1 for survived or 0 for perished
Prediction <- ifelse(titanic_test$pred < 0.5,
                     0,
                     1)

submit <- data.frame(PassengerId = titanic_test$PassengerId, Survived = as.vector(Prediction))

write.csv(submit, file = "kagglesubmission_logit.csv", row.names = FALSE)
