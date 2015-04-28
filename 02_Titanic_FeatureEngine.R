# load required packages
library(dplyr)
library(RCurl)
library(rpart)

#Richard Shanahan  
#https://github.com/rjshanahan  
#6 April 2015
  
#INFS 5098: PROJECT: R code to cleanse Kaggle Titanic data set inc. feature engineering

##### FEATURE ENGINEERING_NEW VARIABLES#####

##### 0. build Titanic dataframes ######

git_path <- "https://raw.github.com/rjshanahan/INFS5098_KaggleTitanic/master/"
train_csv <- "train.csv"
test_csv <- "test.csv"
missing_types <- c("NA", "")

titanic_train_raw <- read.csv(getURI(paste(git_path, train_csv, sep="")), 
                              colClasses=c(
                                'integer',   # PassengerId
                                'factor',    # Survived 
                                'factor',    # Pclass
                                'character', # Name
                                'factor',    # Sex
                                'numeric',   # Age
                                'integer',   # SibSp
                                'integer',   # Parch
                                'character', # Ticket
                                'numeric',   # Fare
                                'character', # Cabin
                                'factor'),   # Embarked
                              na.strings=missing_types                          
                              )

titanic_train <- titanic_train_raw

# build Titanic TEST dataframe from site

titanic_test_raw <- read.csv(getURI(paste(git_path, test_csv, sep="")), 
                              colClasses=c(
                                'integer',   # PassengerId
                                #'factor',    # Survived - not present in TEST
                                'factor',    # Pclass
                                'character', # Name
                                'factor',    # Sex
                                'numeric',   # Age
                                'integer',   # SibSp
                                'integer',   # Parch
                                'character', # Ticket
                                'numeric',   # Fare
                                'character', # Cabin
                                'factor'),   # Embarked
                              na.strings=missing_types                          
)

titanic_test <- titanic_test_raw


# OR LOCAL IMPORT
titanic_train <- read.csv('/yourfilepath/train.csv',
                         header=T,
                         sep=",",
                         quote='"',
                         colClasses=c(
                                      'integer',   # PassengerId
                                      'factor',    # Survived 
                                      'factor',    # Pclass
                                      'character', # Name
                                      'factor',    # Sex
                                      'numeric',   # Age
                                      'integer',   # SibSp
                                      'integer',   # Parch
                                      'character', # Ticket
                                      'numeric',   # Fare
                                      'character', # Cabin
                                      'factor'     # Embarked
                         ),
                         strip.white=T,
                         stringsAsFactors=F,
                         fill=T)

## build Titanic TEST dataframe from data
titanic_test <- read.csv('/yourfilepath/test.csv',
                         header=T,
                         sep=",",
                         quote='"',
                         colClasses=c(
                           'integer',   # PassengerId
                           #'factor',    # Survived - not present in TEST
                           'factor',    # Pclass
                           'character', # Name
                           'factor',    # Sex
                           'numeric',   # Age
                           'integer',   # SibSp
                           'integer',   # Parch
                           'character', # Ticket
                           'numeric',   # Fare
                           'character', # Cabin
                           'factor'     # Embarked
                         ),
                         strip.white=T,
                         stringsAsFactors=F,
                         fill=T)

# create placeholder for survived in test
titanic_test$Survived <- NA

#combine dataframes for feature engineering
titanic_combi <- rbind(titanic_train, titanic_test) 

############ 0. clean up NA values #############

# 1. Age NAs

#update combi
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                data=titanic_combi[!is.na(titanic_combi$Age),], method="anova")

titanic_combi$Age[is.na(titanic_combi$Age)] <- predict(Agefit, titanic_combi[is.na(titanic_combi$Age),])


# 2. Embarked NAs

#update combi
which(titanic_combi$Embarked == '')
titanic_combi$Embarked[c(62,830)] = "S"
titanic_combi$Embarked <- factor(titanic_combi$Embarked)

# 3. Fare NAs

#update combi
which(is.na(titanic_combi$Fare))
titanic_combi$Fare[1044] <- median(titanic_combi$Fare, na.rm=TRUE)


########## 1. Name Releated Variables ##########

##########  1.1 add new variable for salutation ########## 
titanic_combi$title <- sapply(titanic_combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})

#inspect
titanic_combi[1:10,] %>%
select(Name, title, Survived)

## title consolidation

# define dataframe for title reclassifier lookup
title_type <- c('Capt'='Nobility',
              'Col'='Nobility',
              'Don'='Nobility',
              'Dona'='Nobility',
              'Dr'='Nobility',
              'Jonkheer'='Nobility',
              'Lady'='Nobility',
              'Major'='Nobility',
              'Rev'='Respected',
              'Sir'='Respected',
              'the Countess'='Nobility',
              'Mme'='Mrs',
              'Ms'='Mrs',
              'Mrs'='Mrs',
              'Mlle'='Miss',
              'Miss'='Miss',
              'Mr'='Mr',
              'Master'='Master')

#make sure title is factor
titanic_combi$title <- as.factor(titanic_combi$title)

#recode variable
titanic_combi$title <- title_type[titanic_combi$title]

#coerce
titanic_combi$title <- as.factor(titanic_combi$title)

#inspect
table(titanic_combi$title)

########## 1.2 add new variable for last name ########## 
titanic_combi$lastname <- sapply(titanic_combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

#inspect
titanic_combi[1:10,] %>%
select(Name, lastname , Survived)

########## 1.3 add new variable for nickname ########## 
titanic_combi <- mutate(titanic_combi, nickname = as.integer(ifelse(!grepl('\\(', Name) == T &
                                                         grepl('"', Name) == T , 1, 0)))
#inspect
titanic_combi[1:10,] %>%
select(Name, title, nickname, Survived) %>%
filter(!grepl('\\(', Name) & grepl('"', Name))

########## 1.4 add new variable for alternative name ########## 
titanic_combi <- mutate(titanic_combi, altname = as.integer(ifelse(grepl('[()]', Name) == T, 1, 0)))

#inspect
titanic_combi[1:10,] %>%
select(Name, title, altname, Survived) %>%
filter(grepl('[()]', Name))

##########  1.5 add new variable for 'berg' in passenger's name ########## 
titanic_combi <- mutate(titanic_combi, iceberg = as.integer(ifelse(grepl('berg', Name) == T, 1, 0)))

#inspect
titanic_combi %>%
select(Name, iceberg, Survived) %>%
filter(grepl('berg', Name))

##########  2. Cabin Releated Variables ########## 

########## 2.1 add variable indicating pasenger deck given cabin id ########## 
# A to G is top deck to bottom deck
# issue with same passenger, multiple decks - interim solution: takes first character only

#coerce
titanic_combi$Cabin <- as.character(titanic_combi$Cabin)

#regex to split deck
titanic_combi <- mutate(titanic_combi, deck = (substr(gsub('\\s|[0-9]', "", Cabin), 1, 1)))
#replace missing values
titanic_combi$deck[ which((titanic_combi$deck == ""))] <- "unk"

#coerce
titanic_combi$Cabin <- as.factor(titanic_combi$Cabin)
          
#inspect
titanic_combi[1:20,] %>%
select(Name, Cabin, deck, Survived)

########## 2.2 add variable to represent sub-class ########## 
#assumes: 1st class on A deck better than 1st class on B deck

titanic_combi <- mutate(titanic_combi, subclass = as.factor(paste(Pclass, deck, sep="")))

#inspect
titanic_combi[1:20,] %>%
  select(Name, Pclass, Cabin, deck, subclass, Survived)
#inspect
table(titanic_combi$subclass, titanic_combi$Survived)

##########  2.3 add variable to indicate passengers side - port or starboard ########## 

is.even <- function(x) x %% 2 == 0
is.odd <- function(x) x %% 2 != 0
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))}

#coerce
titanic_combi$Cabin <- as.character(titanic_combi$Cabin)

titanic_combi <- mutate(titanic_combi, side = (ifelse(is.even(as.integer(substrRight(Cabin, 1))),
                                                                      "port",
                                                                      ifelse(is.odd(as.integer(substrRight(Cabin, 1))),
                                                                        "starboard",
                                                                        "unknown"))))
#replace missing values
titanic_combi$side[ which(is.na(titanic_combi$side))] <- "unknown"

#coerce
titanic_combi$Cabin <- as.factor(titanic_combi$Cabin)
titanic_combi$side <- as.factor(titanic_combi$side)
titanic_combi$deck <- as.factor(titanic_combi$deck)


#inspect
titanic_combi[1:20,] %>%
  select(Name, Pclass, Cabin, deck, side, Survived)
table(titanic_combi$side, titanic_combi$Survived)

##########  3. Family Releated Variables ##########  

##########  3.1 add variable for familysize ##########

titanic_combi <- mutate(titanic_combi, familysize = SibSp + Parch + 1)

#inspect
titanic_combi %>%
  select(Name, SibSp, Parch, familysize, Survived) %>%
  filter(SibSp>0 & Parch>0)

table(titanic_combi$familysize, titanic_combi$Survived)
subset(titanic_combi, titanic_combi$familysize == 11)

##########  3.2 add variable for fare per person ##########  
# account for group purchases for family

titanic_combi <- mutate(titanic_combi, farepp = Fare / familysize)

#inspect
titanic_combi[1:20,] %>%
  select(Name, Fare, familysize, farepp, Survived) %>%
  filter(familysize > 1)

  
########## 3.3 add variable for long or short term married couple ##########  
# assumes that if passenger age is less than average 'married' passenger age then 'short' term married
# note: average age of married folk - assuming SibSp=1 indicates spouse travelling only - Age relatively normally distributed

avgmarriage <- mean((subset(titanic_combi, titanic_combi$SibSp == 1 && !is.null(titanic_combi$Age))$Age), na.rm=T)

titanic_combi <- mutate(titanic_combi, marriagelength = as.factor(ifelse(Age > avgmarriage,
                                                               "Long",
                                                               "Short")))
#inspect
titanic_combi[1:20,] %>%
  select(Name, SibSp, Parch, familysize, marriagelength, Survived)

table(titanic_combi$marriagelength, titanic_combi$Survived)

########## 3.4 add variable for family with young children or old children ########## 
# assumes that if passenger age is greater than average passenger with children age then 'old' children, otherwise 'young'
# note: average age of people with children - assuming Parch is not 0 and SibSp=1 - double counting of parents an issue - Age relatively normally distributed

avgchildage <- mean((subset(titanic_combi, titanic_combi$Parch > 0 & titanic_combi$SibSp == 1 & !is.null(titanic_combi$Age))$Age), na.rm=T)

titanic_combi <- mutate(titanic_combi, childage = as.factor(ifelse(Age > avgchildage,
                                                                         "Old",
                                                                         "Young")))
#inspect
titanic_combi[1:20,] %>%
  select(Name, SibSp, Parch, familysize, marriagelength, childage, Survived)

table(titanic_combi$childage, titanic_combi$Survived)

#additional coercion
titanic_combi$familysize <- as.factor(titanic_combi$familysize)
titanic_combi$childage <- as.factor(titanic_combi$childage)
titanic_combi$marriagelength <- as.factor(titanic_combi$marriagelength)

##########  5. Fare Related Variables ########## 

########## 5.1 add variable for fare classification ########## 
titanic_combi <- mutate(titanic_combi, faregroup = as.factor(ifelse(Fare >= 30,
                                                          '30+',
                                                          ifelse(Fare < 30 & Fare > 20,
                                                                 '20-30',
                                                                 ifelse(Fare < 20 & Fare > 10,
                                                                        '10-20',
                                                                        '<10')))))
#inspect
titanic_combi[1:30,] %>%
  select(Name, Fare, faregroup, Survived)

#coerce
titanic_combi$faregroup <- as.factor(titanic_combi$faregroup)


########## 5.2 add variable to indicate passenger type by region using 'Embarked' ########## 
titanic_combi <- mutate(titanic_combi, classregion = as.factor(paste(Embarked, Pclass, sep="")))

#inspect
titanic_combi[1:20,] %>%
  select(Name, Embarked, subclass, classregion, Survived)

table(titanic_combi$classregion, titanic_combi$Survived)


############ 6. split dataframes for TRAIN and TEST ###############


titanic_train <- titanic_combi[1:891,]
titanic_test <- titanic_combi[892:1309,]

str(titanic_train)
str(titanic_test)

############ 7. output TRAIN and TEST data frames to csv in working directory ###############

write.csv(titanic_train, file = "titanic_train.csv", row.names = FALSE)
write.csv(titanic_test, file = "titanic_test.csv", row.names = FALSE)
