# machine learning
# binary classification
# decision tree model
# predict diabetes dataset

# load package
install.packages("tidyverse")
install.packages("mlbench")
install.packages("rpart")
install.packages("rpart.plot")
library(tidyverse)
library(mlbench)
library(rpart)
library(rpart.plot)

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
TreeModel <- rpart(diabetes ~ . , 
                   data = train_data , 
                   method =  "class")
TreeModel

# test model
p <- predict(TreeModel, newdata = test_data, type = "class")
p[1:10]
p[11:20]

# evaluate model (prediction == actual)
mean(p == test_data$diabetes)

# confusion matrix
table(p, test_data$diabetes, dnn = c("predicted", "actual"))

# plot tree
rpart.plot(TreeModel)
