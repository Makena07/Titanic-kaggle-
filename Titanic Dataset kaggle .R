 
# Definitions of variables
# titanic.train:  data.frame of the titanic data. Used to train the machine learning model
# titanic.test:   data.frame of the titanic data. Used to test the machine learning model. 
# titanic.full:   data.frame which is the result of the merge of titanic.train and titanic.tes
# 

install.packages("randomForest")
library(randomForest)


# Set working directory
setwd("C:/Users/janem/Downloads/janekaggle")

# Import csv file and define titanic.train and titanic.test
titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test  <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

# Check dimensions, name of columns=features, and structure of titanic.train and titanic.test
ncol(titanic.train)
ncol(titanic.test)
names(titanic.train)
names(titanic.test)
str(titanic.test)

# Create a common column IsTrainset in titanic.train and titanic.test,
# in order to differenciate them in the merged table titanic.full.
titanic.train$IsTrainset <- TRUE
titanic.test$IsTrainset <- FALSE

# Create Survived column in titanic.test and fill in with NA.
titanic.test$Survived <- NA

# Check columns of titanic.train and titanic.test before merging
names(titanic.train)
names(titanic.test)


# Create titanic.full by merging datasets titanic.train and titanic.test.
titanic.full <- rbind(titanic.train,  titanic.test)
head(titanic.full)
dim(titanic.train)
dim(titanic.test)
dim(titanic.full)

# Clean data of titanic.train AND titanic.test through titanic.full

# Check structure of titanic.full
# Change type of Pclass, Sex and Embarked as factor in titanic.full

str(titanic.full)
titanic.full$Pclass   <- as.factor(titanic.full$Pclass)
titanic.full$Sex      <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- factor(titanic.full$Embarked, exclude = NA)
str(titanic.full)

##Clean data - Embarked in titanic.full

# Show lines in titanic.full where Embarked data is missing and substitute the missing data by S.
# Check result
table(titanic.full$Embarked == "")

titanic.full[ titanic.full$Embarked == "", "Embarked"] <- "S"

table(titanic.full$Embarked == "")

##Clean data - Age IN titanic.full 


table(is.na(titanic.full$Age))

upper.whisker.female.age <- boxplot.stats(titanic.full[ titanic.full$Sex == "female" , "Age" ])$stats[5]
upper.whisker.male.age   <- boxplot.stats(titanic.full[ titanic.full$Sex == "male" , "Age" ])$stats[5]

outlier.booleanFilter.female.age <- titanic.full$Age < upper.whisker.female.age
outlier.booleanFilter.male.age   <- titanic.full$Age < upper.whisker.male.age
  
age.equation = "Age ~ Pclass + Sex + Fare + SibSp + Parch + Embarked"
female.age.predictMissingData.model <- lm( formula = age.equation, data = titanic.full[outlier.booleanFilter.female.age, ])
male.age.predictMissingData.model   <- lm( formula = age.equation, data = titanic.full[outlier.booleanFilter.male.age, ])
  
missingAge.female.row <- titanic.full[is.na(titanic.full$Age) & titanic.full$Sex == "female", c("Pclass", "Sex", "Fare", "SibSp", "Parch", "Embarked")]
missingAge.male.row   <- titanic.full[is.na(titanic.full$Age) & titanic.full$Sex == "male", c("Pclass", "Sex", "Fare", "SibSp", "Parch", "Embarked")]
  
prediction.age.female <- predict( female.age.predictMissingData.model, newdata = missingAge.female.row)
prediction.age.male <-   predict( male.age.predictMissingData.model, newdata = missingAge.male.row)
  
#Substitute missing values by the predictions, and check that no more rows with missing exists
titanic.full[ is.na(titanic.full$Age) & titanic.full$Sex == "female", "Age"] <- prediction.age.female
titanic.full[ is.na(titanic.full$Age) & titanic.full$Sex == "male", "Age"]   <- prediction.age.male
  
table(is.na(titanic.full$Age))

##Clean data - Fare IN titanic.full 

table(is.na(titanic.full$Fare))
  
upper.whisker.fare <- boxplot.stats(titanic.full$Fare)$stats[5]
outlier.booleanFilter.fare <- titanic.full$Fare < upper.whisker.fare
  
fare.equation = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
fare.predictMissingData.model <- lm( formula = fare.equation, data = titanic.full[outlier.booleanFilter.fare, ])
  
missingFare.row <- titanic.full[is.na(titanic.full$Fare), c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked")]
prediction.fare <- predict( fare.predictMissingData.model, newdata = missingFare.row)
 
titanic.full[ is.na(titanic.full$Fare), "Fare"] <- prediction.fare

table(is.na(titanic.full$Fare))



##Split data back from partially cleaned titanic.full into titanic.train AND titanic.test
##Update titanic.train and titanic.test with the cleaned data from titanic.full
titanic.train <- titanic.full[titanic.full$IsTrainset == TRUE, ]
titanic.test  <- titanic.full[titanic.full$IsTrainset == FALSE, ]

##Clean Survived data of titanic.train by converting as factor.
##Survived data in titanic.test = NA and cannot be considered as factor.
titanic.train$Survived <- as.factor(titanic.train$Survived)
str(titanic.train)

#RUN randomForest WITH titanic.full

#Create parameters for random forest, and convert into formula type.
survived.equation <- "Survived ~  Sex + Age + SibSp + Parch + Fare + Embarked" # Select some feature
# survived.equation <- "Survived ~ ." # Use all features, but it does not work
survived.formula  <- as.formula(survived.equation)

# Create randomForest
titanic.model <- randomForest(formula = survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodsize = 0.01*nrow(titanic.test), importance = TRUE)

# feature.equation <- "Pclass + Sex + SibSp + Parch + Fare + Embarked" 

# Store the prediction results in Survived
Survived <- predict(titanic.model, newdata = titanic.test)

# Plot importance and variance
importance(titanic.model)
varImpPlot(titanic.model)




# Create empty data.frame output.df to store results with PassengerID and Survived
output.df <- data.frame(PassengerId = integer(nrow(titanic.test)), Survived=factor(nrow(titanic.test)))
output.df$PassengerId <- titanic.test$PassengerId
output.df$Survived <- Survived

# Export the results output.df into csv file

write.csv(output.df, file = "kaggle_submission.csv", row.names = FALSE)



#END