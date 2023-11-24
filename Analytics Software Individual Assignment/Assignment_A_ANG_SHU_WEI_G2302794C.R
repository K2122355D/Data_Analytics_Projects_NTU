#Chosen classification using C5.0 decision trees technique
#C5.0 decision trees technique is a common decision tree and classification rule learner
#Test using the data "credit.csv" provided in the 'Machine Learning with R, 2nd Edition" book
#Chapter 5, pages 136- 149

getwd()
credit <- read.csv("credit.csv", stringsAsFactors= TRUE)

#understanding of the dataset
str(credit)
table(credit$checking_balance)
table(credit$savings_balance)
summary(credit$months_loan_duration)
summary(credit$amount)
table(credit$default)

#Data preparation – creating random training and test datasets

set.seed(123)
train_sample <- sample(1000,900)
str(train_sample)
credit_train <- credit[train_sample, ]
credit_test <- credit[-train_sample, ]
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

#Step 3 – training a model on the data
#install.packages("C50")
library(C50)
?C5.0Control
credit_model <- C5.0(credit_train[-17], credit_train$default)
credit_model
summary(credit_model)

#Step 4 – evaluating model performance
credit_pred <- predict(credit_model, credit_test)
#install.packages('gmodels')
library(gmodels) # for the cross tab table
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))


#Step 5 – improving model performance
credit_boost10 <- C5.0(credit_train[-17], credit_train$default,trials = 10)
credit_boost10
summary(credit_boost10)
credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#Making mistakes more costlier than others
matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")
matrix_dimensions
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2,
                     dimnames = matrix_dimensions)
error_cost
credit_cost <- C5.0(credit_train[-17], credit_train$default,costs = error_cost)
credit_cost

credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#End of using the dataset from the book

#Start of using another dataset (obesity) to test this decision tree (classification)

obesity <- read.csv("ObesityDataSet_raw_and_data_sinthetic.csv", stringsAsFactors= TRUE)
str(obesity)
table(obesity$Age)
table(obesity$NObeyesdad)
table(obesity$MTRANS)
prop.table(table(obesity$NObeyesdad))
summary(obesity$FCVC)
summary(obesity$Age)
summary(obesity$Height)
summary(obesity$Weight)
table(obesity$SMOKE)
summary(obesity)

head(obesity)

#Data exploration
#install.packages("ggplot2")
library(ggplot2)

hist(obesity$Weight, breaks=10)
hist(obesity$Height, breaks=10)
#surprisingly more women than men weigh heavier as seen in weights between 120-150kg
#however the heaviest is a male but this person is also fairly tall nearing to 1.9m
ggplot(obesity) + aes(Weight, Height, col = Gender) + geom_point()

#many do not smoke
ggplot(obesity) + aes(Weight, Height, col = SMOKE) + geom_point()

#strong relationship with family members who have someone obese
ggplot(obesity) + aes(Weight, Height, col = family_history_with_overweight) + geom_point()+
theme(
    text = element_text(size = 12)
  )

#having more small meals are better in having few large meals in terms of weight control
ggplot(obesity) + aes(Weight, Height, col = CAEC) + geom_point()+ labs(
  title = "Scatter Plot between Height and weight",
  x = "Weight",
  y = "Height",
  color = "Eat food between meals"
) 

#having walking or bike as means of transportation helps in preventing one from being too obese;
#those on public transport or automobile are likely to gain more weight
ggplot(obesity) + aes(Weight, Height, col = MTRANS) + geom_point()


#Data preparation – creating random training and test datasets
set.seed(123)
train_ratio <- 0.9
train_sample <- sample(nrow(obesity),train_ratio*nrow(obesity))
str(train_sample)
obesity_train <- obesity[train_sample, ]
obesity_test <- obesity[-train_sample, ]

str(obesity_train)
str(obesity_test)

prop.table(table(obesity_train$NObeyesdad))
prop.table(table(obesity_test$NObeyesdad))

#Step 3 – training a model on the data
#install.packages("C50")
library(C50)

#The 17th column in obesity_train is the NObeyesdad class variable, so we need to exclude it #from the training data frame, but supply it as the target factor vector for #classification.

obesity_model <- C5.0(obesity_train[-17], obesity_train$NObeyesdad)
obesity_model
summary(obesity_model)


#Step 4 – evaluating model performance
obesity_pred <- predict(obesity_model, obesity_test)
#install.packages('gmodels')
library(gmodels) # for the cross tab table
CrossTable(obesity_test$NObeyesdad, obesity_pred, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual class', 'predicted class'))

#Step 5 – improving model performance
obesity_boost10 <- C5.0(obesity_train[-17],obesity_train$NObeyesdad,trials = 10)
obesity_boost10
summary(obesity_boost10)
obesity_boost_pred10 <- predict(obesity_boost10, obesity_test)
CrossTable(obesity_test$NObeyesdad, obesity_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual class', 'predicted class'))

#Tuning of parameters
obesity_tuning_model <- C5.0(formula = NObeyesdad ~ ., 
                             data = obesity_train, 
                             trials = 10, winnow = TRUE, 
                             CF = 0.25, minCases = 100, fuzzyThreshold = 0.5)

#Evaluation of model after tuning of parameters
summary(obesity_tuning_model)
obesity_tuning_model_pred10 <- predict(obesity_tuning_model, obesity_test)
CrossTable(obesity_test$NObeyesdad, obesity_tuning_model_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual class', 'predicted class'))

#Making mistakes more costlier than others
#setting up error_cost matrix (version 1)
#give cost to all the 7 factor levels
matrix_dimensions <- list(c("insufficient_weight", "Normal_Weight","Obesity_Type_I","Obesity_Type_II","Obesity_Type_III","Overweight_Level_I","Overweight_Level_II"),c("insufficient_weight", "Normal_Weight","Obesity_Type_I","Obesity_Type_II","Obesity_Type_III","Overweight_Level_I","Overweight_Level_II"))

names(matrix_dimensions) <- c("predicted", "actual")

error_cost <- matrix(c(0,1,2,3,4,5,6,2,0,3,4,5,6,7,3,1,0,2,3,4,5,4,1,1,0,2,3,4,5,1,2,1,0,2,3,6,1,3,2,1,0,2,7,1,4,3,2,1,0), nrow = 7,byrow= TRUE,
                     dimnames = matrix_dimensions)

error_cost
obesity_cost <- C5.0(obesity_train[-17], obesity_train$NObeyesdad,costs = error_cost)

obesity_cost_pred <- predict(obesity_cost, obesity_test)
CrossTable(obesity_test$NObeyesdad, obesity_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual class', 'predicted class'))

# Check if a tree is available
if (exists("obesity_cost_tree")) {
  # Predict using the C5.0 tree model
  obesity_cost_pred <- predict(obesity_cost_tree,new_data =obesity_test)
} else if (exists("obesity_cost_rules")) {
  # Predict using rules if available
  obesity_cost_pred <- predict(obesity_cost_rules, new_data = obesity_test)
} else {
  # Handle the case where neither a tree nor rules are available
  stop("Neither a tree nor rules are available for prediction.")
}

#Making mistakes more costlier than others
#setting up error_cost matrix (version 2)
#Simplifying the error_cost matrix into 3 factor levels instead of 7
#Give cost to all the 3 factor levels

matrix_dimensions_v2 <- list(c("insufficient_weight", "Normal_Weight","Obesity_or_Overweight"),c("insufficient_weight", "Normal_Weight","Obesity_or_Overweight"))

error_cost_v2 <- matrix(c(0,1,2,1,0,1,2,1,0), nrow = 3,byrow= TRUE,
                     dimnames = matrix_dimensions_v2)

names(matrix_dimensions_v2) <- c("predicted", "actual")
matrix_dimensions_v2
error_cost_v2

#install.packages("dplyr")
library(dplyr)

obesity_train$NObeyesdad1 <- ifelse(obesity_train$NObeyesdad %in% c("Obesity_Type_I", "Obesity_Type_II","Obesity_Type_III","Overweight_Level_I","Overweight_Level_II"), "Obesity_or_Overweight", ifelse(obesity_train$NObeyesdad %in% "Normal_Weight", "Normal_Weight","Insufficient_Weight"))
obesity_train$NObeyesdad1 <- as.factor(obesity_train$NObeyesdad1)

obesity_train$NObeyesdad1
class(obesity_train$NObeyesdad1)
str(obesity_train$NObeyesdad1)

summary(obesity_train)
str(obesity_test)

obesity_test$NObeyesdad1 <- ifelse(obesity_test$NObeyesdad %in% c("Obesity_Type_I", "Obesity_Type_II","Obesity_Type_III","Overweight_Level_I","Overweight_Level_II"), "Obesity_or_Overweight", ifelse(obesity_test$NObeyesdad %in% "Normal_Weight", "Normal_Weight","Insufficient_Weight"))
obesity_test$NObeyesdad1 <- as.factor(obesity_test$NObeyesdad1)
str(obesity_test$NObeyesdad1)

obesity_cost_v2 <- C5.0(obesity_train[-18], obesity_train$NObeyesdad1,costs = error_cost_v2)

obesity_cost_pred_v2 <- predict(obesity_cost_v2, obesity_test)
CrossTable(obesity_test$NObeyesdad, obesity_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual class', 'predicted class'))

# Check if a tree is available
if (exists("obesity_cost_tree")) {
  # Predict using the C5.0 tree model
  obesity_cost_pred_v2 <- predict(obesity_cost_tree, newdata = obesity_test)
} else if (exists("obesity_cost_rules")) {
  # Predict using rules if available
  obesity_cost_pred_v2 <- predict(obesity_cost_rules, new_data = obesity_test)
} else {
  # Handle the case where neither a tree nor rules are available
  stop("Neither a tree nor rules are available for prediction.")
}




#End