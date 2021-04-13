#LOAD DATA SET
rm(list=ls()) 

## import file
filename<-file.choose()
data <-  read.csv(filename)
View(data)

#data=data %>% mutate_if(is.character, as.factor)

#PRINT Data types
sapply(data, typeof)

data <- subset(data, select = -c(EMP_ID, JOBCODE, REFERRAL_SOURCE,HIRE_MONTH,TERMINATION_YEAR))
summary(data)

#SPlit data
library(caret)
inTrain <- createDataPartition(data$STATUS,p=0.75,list = FALSE)
train_data <- data[inTrain,]
test_data <- data[-inTrain,]

set.seed(20)
library(e1071)

svm.model <- svm(train_data$STATUS~., data = subset(train_data, select = -c(STATUS)))
svm.pred <- predict(svm.model,  subset(test_data, select = -c(STATUS)) )

table(actual=test_data$STATUS,svm.pred )

SVM_wrong<- (test_data$STATUS!=svm.pred)
error_rate<-sum(SVM_wrong)/length(SVM_wrong)
error_rate
accuracy<-100-(error_rate*100)
accuracy
