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
#install.packages("caret")
library(caret)
library(ggplot2)

#KNN MODEL
#data$HRLY_RATE <- as.numeric(data$HRLY_RATE)
library(dplyr)
data=cbind(subset(data, select = -c(STATUS)) %>% mutate_if(is.factor, as.integer),data['STATUS'])
#data=cbind(data[0:3],data['STATUS'])

#SPlit data
inTrain <- createDataPartition(data$STATUS,p=0.75,list = FALSE)
train_data <- data[inTrain,]
test_data <- data[-inTrain,]

ggplot(train_data,aes(STATUS,fill=STATUS))+geom_bar()
prop.table(table(train_data$STATUS)) #Percentage of Attrition



## install and import required packages
installed.packages()
#install.packages(kknn)
library(kknn)

## apply knn for k = 3
k3 <- kknn(formula = STATUS~., test_data, train_data, k=3)
newk3 <- fitted((k3))
table(train_data$STATUS, newk3)

## calculate error rate and accuracy
error<-sum(newk3 != train_data$STATUS) #error
error_rate <- sum(newk3 != train_data$STATUS)/length(newk3 != train_data$STATUS)
error_rate
accuracy<-100-(error_rate*100)
accuracy

## apply knn for k = 5
k5 <- kknn(formula = STATUS~., test_data, train_data, k=5)
newk5 <- fitted((k5))
table(train_data$STATUS, newk5)

## calculate error rate and accuracy
error<-sum(newk5 != train_data$STATUS) #error
error_rate <- sum(newk5 != train_data$STATUS)/length(newk5 != train_data$STATUS)
error_rate
accuracy<-100-(error_rate*100)
accuracy

## apply knn for k = 10
k10 <- kknn(formula = STATUS~., test_data, train_data, k=10)
newk10 <- fitted((k10))
table(train_data$STATUS, newk10)

## calculate error rate and accuracy
error<-sum(newk10 != train_data$STATUS) #error
error_rate <- sum(newk10 != train_data$STATUS)/length(newk10 != train_data$STATUS)
error_rate
accuracy<-100-(error_rate*100)
accuracy

