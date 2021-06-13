# machine learning
# binary classification
# logistic regression
# predict diabetes dataset

# load package
install.packages("tidyverse")
install.packages("mlbench")
library(tidyverse)
library(mlbench)

# load dataset
data(PimaIndiansDiabetes)
diabetes <- tibble(PimaIndiansDiabetes)

# preview dataset
glimpse(diabetes)
View(diabetes)
head(diabetes)
tail(diabetes)

# review data types
str(diabetes)
diabetes$diabetes <- as.factor(diabetes$diabetes)
diabetes

# table() count frequency
table(diabetes$diabetes) / nrow(diabetes)

# split dataset
set.seed(42)
n <- nrow(diabetes)
train_id <- sample(1:n , 0.8*n)
train_data <- diabetes[train_id, ]
test_data <- diabetes[-train_id, ]

# train model
logisticModel <- glm(diabetes ~ . , 
                     data = train_data , 
                     family =  "binomial")

# test model
p <- predict(logisticModel, newdata = test_data, type = "response")
p[1:10]
p[11:20]
prediction <- ifelse(p > 0.5 , "pos" , "neg" )
prediction[1:20]

# evaluate model (prediction == actual)
prediction == test_data$diabetes
mean(prediction == test_data$diabetes)

# confusion matrix
table(prediction, test_data$diabetes, dnn = c("predicted", "actual"))
