#LOAD DATA SET

rm(list=ls()) 

## import file
filename<-file.choose()
data <-  read.csv(filename)
View(data)
raw <- data

data$ETHNICITY <- as.character(data$ETHNICITY)
data$REFERRAL_SOURCE <- as.character(data$REFERRAL_SOURCE)
#filter(data, ETHNICITY == " " | REFERRAL_SOURCE == "")
data[data$ETHNICITY == " ", "ETHNICITY"] <- "Unknown"
data[data$REFERRAL_SOURCE == "", "REFERRAL_SOURCE"] <- "Unknown"


#install.packages("dplyr")
library(dplyr)
#ans <- data %>% replace(.==NA, "Unknown") # replace with NA

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}
list_of_numeric_cols = c('ANNUAL_RATE','HRLY_RATE','JOB_SATISFACTION','AGE','PERFORMANCE_RATING','PREVYR_1','PREVYR_2','PREVYR_3','PREVYR_4','PREVYR_5')
data_normalized <- as.data.frame(lapply(subset(data, select = list_of_numeric_cols), normalize))
for (i in colnames(data_normalized)){
  data[i] <- data_normalized[i]
}

#REMOVE Highly Correlated Columns to remove biased significance
cor(data_normalized)

## Specify Data Types
data$EMP_ID <- as.character(data$EMP_ID) 
data$ETHNICITY <- as.factor(data$ETHNICITY)
data$SEX <- as.factor(data$SEX)
data$MARITAL_STATUS <- as.factor(data$MARITAL_STATUS)
data$NUMBER_OF_TEAM_CHANGED <- as.factor(data$NUMBER_OF_TEAM_CHANGED)
data$REFERRAL_SOURCE <- as.factor(data$REFERRAL_SOURCE) 
data$HIRE_MONTH <- as.factor(data$HIRE_MONTH)
data$REHIRE <- as.logical(ifelse(data$REHIRE=="TRUE", TRUE, FALSE))
data$IS_FIRST_JOB <- as.logical(ifelse(data$IS_FIRST_JOB=="Y", TRUE, FALSE))
data$TRAVELLED_REQUIRED <- as.logical(ifelse(data$TRAVELLED_REQUIRED=="Y", TRUE, FALSE))
data$DISABLED_EMP <- as.logical(ifelse(data$DISABLED_EMP=="Y", TRUE, FALSE))
data$DISABLED_VET <- as.logical(ifelse(data$DISABLED_VET=="Y", TRUE, FALSE))
data$EDUCATION_LEVEL <- as.factor(data$EDUCATION_LEVEL)
data$REFERRAL_SOURCE <- as.factor(data$REFERRAL_SOURCE)
#data$STATUS <- as.factor(ifelse(data$STATUS=="T", "T", "A")) # T means job was terminated as per i understood 
data$JOB_GROUP <- as.factor(data$JOB_GROUP) 

#PRINT Data types
sapply(data, typeof)

#Removing ID and JOB CODE columns as it does not provide any userful information to predict new data and is unique for all observations
data <- subset(data, select = -c(EMP_ID, JOBCODE, HIRE_MONTH))

#Find Columns with NA Values
colnames(data)[colSums(is.na(data)) > 0] 
#Only Termination Year has NA values and termiantion year is perfectly correlated to STATUS column so drop it in model
data <- subset(data, select = -c(TERMINATION_YEAR,NUMBER_OF_TEAM_CHANGED,REFERRAL_SOURCE,IS_FIRST_JOB))

#NORMALIZE METHOD to remove skew of numeric data points
summary(data)

# ANNUAL_RATE is highly correlated to DAILY_RATE.
data <- subset(data, select = -c(HRLY_RATE))

#Set Random Number Seed
set.seed(123)

#NB MODEL

#SPlit data
inTrain <- createDataPartition(data$STATUS,p=0.75,list = FALSE)
train_data <- data[inTrain,]
test_data <- data[-inTrain,]

#install.packages("caret")
library(caret)
library(ggplot2)
ggplot(train_data,aes(STATUS,fill=STATUS))+geom_bar()
prop.table(table(train_data$STATUS)) #Percentage of Attrition

install.packages("class")
install.packages("e1071")
library(e1071)
library(class)

#Implementing NaiveBayes
modelnaive<- naiveBayes(STATUS ~ ., data = train_data)

#Predicting target class for the Validation set
predictnaive <- predict(modelnaive, test_data)

table(modelnaive=predictnaive,class=test_data$STATUS)

#prop table
prop.table(table(modelnaive=predictnaive,class=test_data$STATUS))

#Error in perdiction of result
wrongprediction<-sum(predictnaive!=test_data$STATUS)

#Error Rate in prediction of Naive Bayes Classifier
wrongpredictionrate<-wrongprediction/length(predictnaive)

##RESULT
print(paste("Total bad Predictions:" , wrongprediction))
print(paste("Error rate :" , wrongpredictionrate))
print(paste("Accuracy :" , 100-(wrongpredictionrate*100)))

