library(corrplot)
library(class)
library(caret)
library(dplyr)
library(Metrics)
library(e1071)
library(C50)
library(gmodels)
library(randomForest)
library(ggforce)
library(tidyverse)
setwd("C:/Users/treei/OneDrive/Desktop/Courses/MH6211-Analytics Software I/Project/")

#I Data Import
machine_failure <- read.csv('machine failure.csv',stringsAsFactors = TRUE)
str(machine_failure)
col_names <- c("ID","ProductID","Type","airtemp","processtemp","rotationalspeed","torque","toolwear","machinefailure","TWF"
              ,"HDF","PWF","OSF","RNF")
colnames(machine_failure) <- col_names
str(machine_failure)

dim(machine_failure)

sum(is.na(machine_failure))

summary(machine_failure)

#Distribution of Target Variable
table(machine_failure$machinefailure)
prop.table(table(machine_failure$machinefailure))
barplot(table(machine_failure$machinefailure))

#Distrinution of machine types

barplot(table(machine_failure$Type))

#Explore the machine failure based on the type of machine
failure_by_type <- table(machine_failure$Type, machine_failure$machinefailure)
failure_by_type
barplot(failure_by_type, beside = TRUE, 
        col = c("green", "red","blue"),
        legend.text = TRUE,
        xlab = "Type", ylab = "Frequency",
        main = "Failure Distribution by Type")
#More number of light machines have machine failure

#Explore the machine failure based on the  of machine
failure_by_type <- table(machine_failure$Type, machine_failure$machinefailure)
failure_by_twf <- table(machine_failure$TWF, machine_failure$machinefailure)
failure_by_pwf <- table(machine_failure$PWF, machine_failure$machinefailure)
failure_by_hdf <- table(machine_failure$HDF, machine_failure$machinefailure)
failure_by_osf <- table(machine_failure$OSF, machine_failure$machinefailure)
failure_by_rnf <- table(machine_failure$RNF, machine_failure$machinefailure)

layout(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE))

barplot(failure_by_type, beside = TRUE, col = c("green", "red","blue"),legend.text = TRUE, main = "Type",xlab = "Type", ylab = "Frequency")
barplot(failure_by_twf, beside = TRUE, col = c("green", "red"),legend.text = TRUE, main = "TWF")
barplot(failure_by_pwf, beside = TRUE, col = c("green", "red"),legend.text = TRUE, main = "PWF")
barplot(failure_by_hdf, beside = TRUE, col = c("green", "red"),legend.text = TRUE, main = "HDF")
barplot(failure_by_osf, beside = TRUE, col = c("green", "red"),legend.text = TRUE, main = "osf")
barplot(failure_by_rnf, beside = TRUE, col = c("green", "red"),legend.text = TRUE, main = "RNF")

#histogram for airtemp
hist(machine_failure$airtemp)

#histogram for processtemp
hist(machine_failure$processtemp)

#corrplot
numeric_vars <- c("airtemp","processtemp","rotationalspeed","torque","toolwear","machinefailure")
corrplot(cor(machine_failure[,names(machine_failure) %in% numeric_vars]),type="upper", method="color",addCoef.col = "black",number.cex = 0.6)

#III Pre-Training The Data
#remove ID and ProductID
machine_failure <- subset(machine_failure,select = c(3:9))
#Change the target to factor variable
machine_failure$machinefailure <- factor(machine_failure$machinefailure)
str(machine_failure)
#change the  type to Integer
machine_failure$Type <- as.integer(machine_failure$Type)
str(machine_failure)

#Splitting the data into training and testing
#We split 70-30 [70% for training and 30% for testing]
set.seed(123)
split <- sample(1:nrow(machine_failure),size = round(0.7 * nrow(machine_failure)))

# Create training and testing sets
train_data <- machine_failure[split, ]
test_data <- machine_failure[-split, ]
train_up <- upSample(x = train_data %>% select(-machinefailure),
                           y = as.factor(train_data$machinefailure),
                          yname = "machinefailure")

print("Train Dataset")
prop.table(table(train_data$machinefailure))
prop.table(table(train_up$machinefailure))
print("Test Dataset")
prop.table(table(test_data$machinefailure))
#Therefore the proportion in training and testing is the same

#IV TRAINING
#KNN CLASSIFICATION
#function
set.seed(123)
str(train_up)

knn1<-knn(train_up[,-7], test_data[,-7], cl=train_up$machinefailure, k=2)
confusionMatrix(knn1,test_data$machinefailure,positive = "1")

#try different k to find the best classifier
ac<-rep(0, 30)
precision <- rep(0, 30)
recall <- rep(0, 30)
for(i in 1:30){
  knn.i<-knn(train_up[,-7], test_data[,-7], cl=train_up$machinefailure, k=i)
  ac[i]<-mean(knn.i ==test_data$machinefailure)
  contingency_table <- table(knn.i,test_data$machinefailure)
  contingency_table
  TP <- contingency_table[2, 2]
  FN <- contingency_table[1, 2]
  FP <- contingency_table[2, 1]
  precision[i] <- TP / (TP + FP)
  recall[i] <- TP / (TP + FN)
  cat("k=", i, " accuracy=", ac[i],"precision=", precision[i],"recall=", recall[i], "\n")
}

##accuracy plot
plot(ac, type="b", xlab="K",ylab="Accuracy")
plot(precision,type="b",xlab="K",ylab = "Precision")
plot(recall,type="b",xlab="K",ylab = "recall")
##k=29
knn2<-knn(train_up[,-7], test_data[,-7], cl=train_up$machinefailure, k=29)
mean(knn2 ==test_data$machinefailure)

confusionMatrix(knn2,test_data$machinefailure,positive = "1")

#SUPPORT VECTOR MACHINE
#build SVM model to the training data set
SVM1 = svm(machinefailure ~ .,
           data = train_data,
           kernel = 'linear')

#predict the test set result
svm1_pred = predict(SVM1, newdata = test_data)

confusionMatrix(svm1_pred,test_data$machinefailure,positive = "1",
                mode = "everything")

# Calculate AUC
Metrics::auc(as.numeric(as.character(test_data$machinefailure)), as.numeric(as.character(svm1_pred)))
############################################################
#dataset without 5 features up sampling
#create upsampling data set
set.seed(123)
#build SVM model to the training data set
SVM2 = svm(machinefailure ~ .,
           data = train_up,
           kernel = 'linear')
#predict the test set result
svm2_pred <- predict(SVM2, newdata = test_data)

confusionMatrix(svm2_pred,test_data$machinefailure,positive = "1",mode = "everything")

# Calculate AUC
Metrics::auc(as.numeric(as.character(test_data$machinefailure)), as.numeric(as.character(svm2_pred)))

#############################model improvement
#build SVM model to the training data set
set.seed(123)
SVM_radial = svm(machinefailure ~ .,
                 data = train_up,
                 kernel = 'radial')
#predict the test set result
svm_radial_predict = predict(SVM_radial, newdata = test_data)
confusionMatrix(svm_radial_predict,test_data$machinefailure,mode = "everything",positive = "1")

# Calculate AUC
Metrics::auc(as.numeric(as.character(test_data$machinefailure)), as.numeric(as.character(svm_radial_predict)))

############################################################
#dataset without 5 features down sampling
#create downsampling data set
set.seed(123)
down_train <- downSample(x = train_data %>% select(-machinefailure),
                         y = as.factor(train_data$machinefailure),
                         yname = "machinefailure")
set.seed(123)
#build SVM model to the training data set
SVM3 = svm(machinefailure ~ .,
           data = down_train,
           kernel = 'linear')
#predict the test set result
svm3_pred = predict(SVM3, newdata = test_data)

confusionMatrix(svm3_pred,test_data$machinefailure,positive = "1",
                mode = "everything")
# Calculate AUC
Metrics::auc(as.numeric(as.character(test_data$machinefailure)), as.numeric(as.character(svm3_pred)))


##############################
#DECISION TREE - CLASSIFICATION#
#dataset without 5 features no up/down sampling (base model)

set.seed(123)
dt1 <- C5.0(train_data[-7], train_data$machinefailure) 
summary(dt1)

#Step 4 evaluating model performance
dt1_pred <- predict(dt1, test_data)

confusionMatrix(dt1_pred,test_data$machinefailure,,positive = "1",mode = "everything")


#Step 5 didn't boost at all in fact accuracy went slightly down after boosting
dt1_boost10 <- C5.0(train_data[-7], train_data$machinefailure,trials = 10)
dt1_boost10
summary(dt1_boost10)
dt1_pred10 <- predict(dt1_boost10, test_data)

#calculate precision and recall

confusionMatrix(dt1_pred10,test_data$machinefailure,positive = "1",
                mode = "everything")


Metrics::auc(as.numeric(as.character(test_data$machinefailure)), 
             as.numeric(as.character(dt1_pred10)))

##############################
#DECISION TREE - CLASSIFICATION#
#dataset without 5 features up sampling
#install.packages("C50") - classification method
set.seed(123)
dt2 <- C5.0(train_up[-7], train_up$machinefailure) 
dt2
summary(dt2)

#Step 4  evaluating model performance
dt2_pred <- predict(dt2, test_data)

confusionMatrix(dt2_pred,test_data$machinefailure,positive = "1",
                mode = "everything")
# classification using the uptrain data accuracy = 97.2%
Metrics::auc(as.numeric(as.character(test_data$machinefailure)), 
             as.numeric(as.character(dt2_pred)))


#Step 5  results are worse after boosting (tried model improvement)
dt2_boost10 <- C5.0(train_up[-7], train_up$machinefailure,trials = 10)
dt2_boost10
summary(dt2_boost10)
dt2_pred10 <- predict(dt2_boost10, test_data)

#calculate precision and recall
confusionMatrix(dt2_pred10,test_data$machinefailure,positive = "1",
                mode = "everything")

Metrics::auc(as.numeric(as.character(test_data$machinefailure)), 
             as.numeric(as.character(dt2_pred10)))
##############################
#DECISION TREE - CLASSIFICATION#
#dataset without 5 features down sampling

#Step 3  training a model on the data
set.seed(123)
dt1b <- C5.0(down_train[-7], down_train$machinefailure) 
dt1b
summary(dt1b)

#Step 4  evaluating model performance
dt1b_pred <- predict(dt1b, test_data)

confusionMatrix(dt1b_pred,test_data$machinefailure,positive = "1",
                mode = "everything")
Metrics::auc(as.numeric(as.character(test_data$machinefailure)), 
             as.numeric(as.character(dt1b_pred)))
# classification using the downtrain data accuracy = 91.7%


#Step 5  results went worse after boosting (tried model improvement)
dt1b_boost10 <- C5.0(down_train[-7], down_train$machinefailure,trials = 10)
dt1b_boost10
summary(dt1b_boost10)
dt1b_pred10 <- predict(dt1b_boost10, test_data)

#calculate precision and recall

confusionMatrix(dt1b_pred10,test_data$machinefailure,positive = "1",
                mode = "everything")


Metrics::auc(as.numeric(as.character(test_data$machinefailure)), 
             as.numeric(as.character(dt1b_pred10)))
#RANDOM FOREST - CLASSIFICATION#
#dataset without 5 features + no up/down sampling (base model)
set.seed(123)
rf <- randomForest(train_data[-7], train_data$machinefailure)
rf_predict <- predict(rf , test_data)
confusionMatrix(rf_predict, test_data$machinefailure, mode = "everything", positive = "1")

Metrics::auc(as.numeric(as.character(test_data$machinefailure)), as.numeric(as.character(rf_predict)))

##############################
#RANDOM FOREST - CLASSIFICATION#
#dataset without 5 features + up sampling
set.seed(123)
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5,verboseIter = TRUE)
set.seed(123)
up_rf<- train(machinefailure~., data = train_up, 
              method = "rf",
              metric = "Accuracy",
              trControl = ctrl)
up_rf
set.seed(123)
up_rf_predict <- predict(up_rf, test_data)
confusionMatrix(up_rf_predict, test_data$machinefailure, mode = "everything", positive = "1")

Metrics::auc(as.numeric(as.character(test_data$machinefailure)), as.numeric(as.character(up_rf_predict)))

##############################
#RANDOM FOREST - CLASSIFICATION#
#dataset without 5 features + down sampling
set.seed(123)
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5,verboseIter = TRUE,)
str(down_train)
set.seed(123)
down_rf <- train(machinefailure~., data = down_train, 
                 method = "rf",
                 metric = "Accuracy",
                 trControl = ctrl)

down_rf_predict <- predict(down_rf, test_data)
confusionMatrix(down_rf_predict, test_data$machinefailure,mode = "everything", positive = '1')

##############################
#LOGISTIC REGRESSION - CLASSIFICATION#
#dataset without 5 features + no up/down sampling (base model)
set.seed(123)
train_data_labels <- train_data$machinefailure
test_data_labels <- test_data$machinefailure
prop.table(table(train_data_labels))
prop.table(table(train_data_labels))

log <- glm(machinefailure ~ ., 
           data = train_data, family = binomial(link = "logit"))
summary(log)
test_pred <- predict(log, test_data,type = 'response')
predicted_labels <- ifelse(test_pred>= 0.5, 1, 0)


predicted_labels <- as.factor(predicted_labels)
mf_test_labels <- as.factor(test_data_labels)
if (!identical(levels(predicted_labels), levels(test_data_labels))) {
  levels(predicted_labels) <- levels(test_data_labels)
}
ct <- confusionMatrix(predicted_labels, test_data_labels, positive = "1",mode = "everything")
ct
Metrics::auc(as.numeric(as.character(test_data$machinefailure)), as.numeric(as.character(predicted_labels)))

##############################
#LOGISTIC REGRESSION - CLASSIFICATION#
#dataset without 5 features + up sampling 

train_up_labels <- train_up$machinefailure
test_data_labels <- test_data$machinefailure
prop.table(table(train_up_labels))
prop.table(table(test_data_labels))

log <- glm(machinefailure ~ ., 
           data = train_up, family = binomial(link = "logit"))
summary(log)
test_pred <- predict(log, test_data,type = 'response')
predicted_labels <- ifelse(test_pred>= 0.5, 1, 0)


predicted_labels <- as.factor(predicted_labels)
mf_test_labels <- as.factor(train_up_labels)
if (!identical(levels(predicted_labels), levels(test_data_labels))) {
  levels(predicted_labels) <- levels(test_data_labels)
}

library(caret)
ct2 <- confusionMatrix(predicted_labels, test_data_labels, positive = "1", mode = "everything")
ct2 

Metrics::auc(as.numeric(as.character(test_data$machinefailure)), as.numeric(as.character(predicted_labels)))

##################################################################################

#V Adding New Features
new <- machine_failure
new$power <-  new$torque * new$rotationalspeed
new$tempdiff <-  new$processtemp - new$airtemp
new$toolwearrate <- new$toolwear/max(new$toolwear)

str(new)

train_data_new <- new[split, ]
test_data_new <- new[-split, ]
train_up_new <- upSample(x = train_data_new %>% select(-machinefailure),
                     y = as.factor(train_data_new$machinefailure),
                     yname = "machinefailure")

print("Train Dataset")
prop.table(table(train_data_new$machinefailure))
prop.table(table(train_up_new$machinefailure))
print("Test Dataset")
prop.table(table(test_data_new$machinefailure))
str(train_data_new)
str(train_up_new)
#Therefore the proportion in training and testing is the same

#VI Feature Engineering
#KNN CLASSIFICATION
set.seed(123)

knn1_new <-knn(train_up_new[,-7], test_data_new[,-7], cl=train_up$machinefailure, k=2)
confusionMatrix(knn1,test_data_new$machinefailure,positive = "1")

#try different k to find the best classifier
ac<-rep(0, 30)
precision <- rep(0, 30)
recall <- rep(0, 30)
for(i in 1:30){
  knn.i<-knn(train_up_new[,-7], test_data_new[,-7], cl=train_up$machinefailure, k=i)
  ac[i]<-mean(knn.i ==test_data_new$machinefailure)
  contingency_table <- table(knn.i,test_data_new$machinefailure)
  contingency_table
  TP <- contingency_table[2, 2]
  FN <- contingency_table[1, 2]
  FP <- contingency_table[2, 1]
  precision[i] <- TP / (TP + FP)
  recall[i] <- TP / (TP + FN)
  cat("k=", i, " accuracy=", ac[i],"precision=", precision[i],"recall=", recall[i], "\n")
}

##accuracy plot
plot(ac, type="b", xlab="K",ylab="Accuracy")
plot(precision,type="b",xlab="K",ylab = "Precision")
plot(recall,type="b",xlab="K",ylab = "recall")
##k=29
knn4<-knn(train_up_new[,-7], test_data_new[,-7], cl=train_up_new$machinefailure, k=4)
mean(knn4 ==test_data$machinefailure)

confusionMatrix(knn4,test_data_new$machinefailure,mode = "everything",positive = "1")
auc(as.numeric(as.character(test_data_new$machinefailure)), as.numeric(as.character(knn4)))

#############################################################################################

#SUPPORT VECTOR MACHINE

#dataset without 5 features + 3 features up sampling
#Make a new dataset with the additional features

str(train_up_new)
#build SVM model to the training data set
set.seed(123)
SVM4 = svm(machinefailure ~ .,
           data = train_up_new,
           kernel = 'linear')
#predict the test set result
svm4_pred = predict(SVM4, newdata = test_data_new)
confusionMatrix(svm4_pred,test_data_new$machinefailure,positive = "1",
                mode = "everything")

Metrics::auc(as.numeric(as.character(test_data$machinefailure)), as.numeric(as.character(svm4_pred)))

#############################model improvement
#build SVM model to the training data set
set.seed(123)
SVM4update = svm(machinefailure ~ .,
                 data = train_up_new,
                 kernel = 'radial')

#predict the test set result
updatesvm4_pred = predict(SVM4update, newdata = test_data_new)
confusionMatrix(updatesvm4_pred,test_data_new$machinefailure,positive = "1",
                mode = "everything")

# Calculate ROC AUC
Metrics::auc(as.numeric(as.character(test_data_new$machinefailure)), as.numeric(as.character(updatesvm4_pred)))

############################################################
#dataset without 5 features + 3 features down sampling
set.seed(123)
down_train_new <- downSample(x = train_data_new %>% select(-machinefailure),
                             y = as.factor(train_data_new$machinefailure),
                             yname = "machinefailure")

#build SVM model to the training data set
set.seed(123)
SVM5 = svm(machinefailure ~ .,
           data = down_train_new,
           kernel = 'linear')
#predict the test set result
svm5_pred = predict(SVM5, newdata = test_data_new)
confusionMatrix(svm5_pred,test_data_new$machinefailure,positive = "1",
                mode = "everything")

# Calculate AUC
Metrics::auc(as.numeric(as.character(test_data_new$machinefailure)), as.numeric(as.character(svm5_pred)))

str(train_up)


##############################
#DECISION TREE - CLASSIFICATION#
#dataset without 5 features + 3 features no up/down sampling (base model)

#Step 3 training a model on the data

dt1_new <- C5.0(train_data_new[-7], train_data_new$machinefailure) 
dt1_new
summary(dt1_new)

#Step 4 evaluating model performance
dt1_new_pred <- predict(dt1_new, test_data_new)


confusionMatrix(dt1_new_pred,test_data_new$machinefailure,positive = "1",
                mode = "everything")


#Step 5 no improvement
dt1_new_pred10 <- C5.0(train_data_new[-7], train_data_new$machinefailure,trials = 10)
dt1_new_pred10
summary(dt1_new_pred10)
dt1_new_pred10 <- predict(dt1_new_pred10, test_data_new)

#calculate precision and recall

confusionMatrix(dt1_new_pred10,test_data_new$machinefailure,positive = "1",
                mode = "everything")



Metrics::auc(as.numeric(as.character(test_data_new$machinefailure)), 
             as.numeric(as.character(dt1_new_pred10)))

##############################
#DECISION TREE - CLASSIFICATION#
#dataset without 5 features + 3 features up sampling
dt2_new <- C5.0(train_up_new[-10], train_up_new$machinefailure) 
dt2_new
summary(dt2_new)

#Step 4 evaluating model performance
dt2_new_pred <- predict(dt2_new, test_data_new)

confusionMatrix(dt2_new_pred,test_data_new$machinefailure,positive = "1")
# classification using the uptrain data accuracy = 98.1%


#Step 5 results are better after boosting (model improvement)
dt2_new_boost10 <- C5.0(train_up_new[-10], train_up_new$machinefailure,trials = 10)
dt2_new_boost10
summary(dt2_new_boost10)
dt2_new_pred10 <- predict(dt2_new_boost10, test_data_new)
#calculate precision and recall

confusionMatrix(dt2_new_pred10,test_data_new$machinefailure,mode = "everything",positive = "1")

Metrics::auc(as.numeric(as.character(test_data_new$machinefailure)), 
             as.numeric(as.character(dt2_new_pred10)))

##############################
#DECISION TREE - CLASSIFICATION#
#dataset without 5 features + 3 features down sampling 

dt1b_new <- C5.0(down_train_new[-10], down_train_new$machinefailure) 
dt1b_new
summary(dt1b_new)


#Step 4 evaluating model performance
dt1b_new_pred <- predict(dt1b_new, test_data_new)
#install.packages('gmodels')
confusionMatrix(dt1b_new_pred,test_data_new$machinefailure,positive = "1",
                mode = "everything")

# classification using the uptrain data accuracy = 89.6%
str(down_train_new)

#Step 5 results are better after boosting (model improvement)
dt1b_new_boost10 <- C5.0(down_train_new[-10], down_train_new$machinefailure,trials = 10)
dt1b_new_boost10
summary(dt1b_new_boost10)
dt1b_new_pred10 <- predict(dt1b_new_boost10, test_data_new)
CrossTable(test_data_new$machinefailure, dt1b_new_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual failure', 'predicted failure'))

#calculate precision and recall

confusionMatrix(dt1b_new_pred10,test_data_new$machinefailure,positive = "1",
                mode = "everything")


Metrics::auc(as.numeric(as.character(test_data_new$machinefailure)), 
             as.numeric(as.character(dt1b_new_pred10)))

##############################

##############################
#RANDOM FOREST - CLASSIFICATION#
#dataset without 5 features + 3 features no up/down sampling (final model)
set.seed(123)
rf_new <- randomForest(train_data_new[-7], train_data_new$machinefailure)
rf_new_predict <- predict(rf_new , test_data_new)
confusionMatrix(rf_new_predict , test_data_new$machinefailure, mode = "everything", positive = '1')

Metrics::auc(as.numeric(as.character(test_data_new$machinefailure)), as.numeric(as.character(rf_new_predict )))

##############################
#RANDOM FOREST - CLASSIFICATION#
#dataset without 5 features + 3 features up sampling (final model)
set.seed(123)
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, verboseIter = TRUE)
set.seed(123)
up_rf_new<- train(machinefailure~., data = train_up_new, 
                  method = "rf",
                  metric = "Accuracy",
                  trControl = ctrl)

up_rf_new
up_rf_new_predict <- predict(up_rf_new, test_data_new)
confusionMatrix(up_rf_new_predict, test_data_new$machinefailure, mode = "everything", positive = '1')

Metrics::auc(as.numeric(as.character(test_data_new$machinefailure)), as.numeric(as.character(up_rf_new_predict)))

##############################
#RANDOM FOREST - CLASSIFICATION#
#dataset without 5 features + 3 features down sampling (final model)
set.seed(123)
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5,verboseIter = TRUE,)

down_rf_new <- train(machinefailure~., data = down_train_new, 
                     method = "rf",
                     metric = "Accuracy",
                     trControl = ctrl)


down_rf_new_predict <- predict(down_rf_new, test_data_new)
confusionMatrix(down_rf_new_predict, test_data_new$machinefailure,mode = "everything", positive = '1')


Metrics::auc(as.numeric(as.character(test_data_new$machinefailure)), as.numeric(as.character(down_rf_new_predict)))

##############################
#LOGISTIC REGRESSION - CLASSIFICATION#
#dataset without 5 features + 3 feature without up/down sampling 
set.seed(123)
train_data_new_labels <- train_data_new$machinefailure
test_data_new_labels <- test_data_new$machinefailure

log2 <- glm(machinefailure ~ ., 
            data = train_data_new, family = binomial(link = "logit"))
summary(log2)
new_test_pred <- predict(log2, test_data_new,type = 'response')
predicted_labels_new <- ifelse(new_test_pred>= 0.5, 1, 0)

predicted_labels_new <- as.factor(predicted_labels_new)
new_test_labels <- as.factor(test_data_new_labels)
if (!identical(levels(predicted_labels_new), levels(test_data_new_labels))) {
  levels(predicted_labels_new) <- levels(test_data_new_labels)
}

ct3 <- confusionMatrix(predicted_labels_new, test_data_new_labels, positive = "1",mode = "everything")
ct3
Metrics::auc(as.numeric(as.character(test_data_new$machinefailure)), as.numeric(as.character(predicted_labels_new)))

##############################
#LOGISTIC REGRESSION - CLASSIFICATION#
#dataset without 5 features + 3 feature up sampling 

set.seed(123)
train_up_new_labels <- train_up_new$machinefailure
test_data_new_labels <- test_data_new$machinefailure

log2 <- glm(machinefailure ~ ., 
            data = train_up_new, family = binomial(link = "logit"))
summary(log2)
new_test_pred <- predict(log2, test_data_new,type = 'response')
predicted_labels_new <- ifelse(new_test_pred>= 0.5, 1, 0)

predicted_labels_new <- as.factor(predicted_labels_new)
test_data_new_labels <- as.factor(test_data_new_labels)
if (!identical(levels(predicted_labels_new), levels(test_data_new_labels))) {
  levels(predicted_labels_new) <- levels(test_data_new_labels)
}

ct4 <- confusionMatrix(predicted_labels_new, test_data_new_labels, positive = "1",mode = "everything")
ct4
Metrics::auc(as.numeric(as.character(test_data_new$machinefailure)), as.numeric(as.character(predicted_labels_new)))
