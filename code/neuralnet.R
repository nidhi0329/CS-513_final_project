#LOAD DATA SET

rm(list=ls()) 

library(lattice)
library(nnet)
library(corrplot)
library(NeuralNetTools)

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
data$STATUS <- as.factor(data$STATUS) # T means job was terminated as per i understood 
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

#SPlit data
inTrain <- createDataPartition(data$STATUS,p=0.75,list = FALSE)
train_data <- data[inTrain,]
test_data <- data[-inTrain,]

#install.packages("caret")
library(caret)
library(ggplot2)
ggplot(train_data,aes(STATUS,fill=STATUS))+geom_bar()
prop.table(table(train_data$STATUS)) #Percentage of Attrition

#Neural Network
library(neuralnet)
nn1 <- nnet(train_data$STATUS~., data = subset(train_data, select = -c(STATUS)),size = 5,maxit = 2000,decay = .01)

options(scipen = 99)
head(nn1$fitted.values)
plotnet(nn1)

## Lets predict the output using test data on the Neural network model nn1 without EmployeeNumber

##Assigning probability and setting a manual cut-off of p=0.5.
## This means that employees who have a probability greater than 0.5 of leaving the organization will be brought into HR's attention which is the aim of this model.
pred_raw_test <- predict(nn1,subset(test_data, select = -c(STATUS)),type = "raw")
cutoff <- floor(pred_raw_test+.5)
plot(pred_raw_test)
abline(a=0.5,b=0,h=0.5)

plot(cutoff)

#Assigning class to the test dataset based on the manual cutoff.
test_data$class_nn1 = ifelse(cutoff==1,"T","A")
test_data$class_nn1 <- as.factor(test_data$class_nn1)

## Evaluate Model Performance using class i.e, Yes/No.
confusionMatrix(test_data$STATUS,test_data$class_nn1)

error<-sum(test_data$class_nn1 != test_data$STATUS) #error
error_rate <- sum(test_data$class_nn1 != test_data$STATUS)/length(test_data$class_nn1 != test_data$STATUS)
error_rate
accuracy<-100-(error_rate*100)
accuracy

