library(ggplot2)
library(caTools)
library(randomForest)
library(glmnet)
library(plyr)
library(dplyr)
train <- read.csv("C:/Users/User/Downloads/research paper/Train_UWu5bXk.csv")
test<- read.csv("C:/Users/User/Downloads/research paper/Test_u94Q5KV.csv")
nRows <- nrow(train)
nCompRows <- sum(complete.cases(train))
nCompRows/nRows



# Check fat levels
train$Item_Fat_Content

# Convert different variations of fat to consistent values
train$Item_Fat_Content <- gsub("LF", "lowfat",train$Item_Fat_Content)
train$Item_Fat_Content <- gsub("low fat", "lowfat",train$Item_Fat_Content)
train$Item_Fat_Content <- gsub("Low Fat", "lowfat",train$Item_Fat_Content)
train$Item_Fat_Content <- gsub("reg", "Regular",train$Item_Fat_Content)
train$Item_Fat_Content <- as.factor(train$Item_Fat_Content)
summary(train$Item_Fat_Content)

# Using mean to replace the missing values in Item_Weight variable
MeanItem_Weight <- mean(train$Item_Weight[!is.na(train$Item_Weight)])
train$Item_Weight[is.na(train$Item_Weight)] <- MeanItem_Weight

# Using regression to replace the zeros in Item_visibility variable
train_temp <- train %>% filter(Item_Visibility != 0)
visibility_model <- lm(Item_Visibility ~ Item_Weight + Item_Fat_Content +
                         Item_Type + Item_MRP +
                         Outlet_Establishment_Year + Outlet_Size +
                         Outlet_Location_Type + Item_Outlet_Sales,
                       data = train_temp)
train$Item_Visibility[train$Item_Visibility == 0] <-
  predict(visibility_model,newdata = train[train$Item_Visibility == 0,])

# Classify missing values in Outlet Size
set.seed(100)
train$Outlet_Size <- as.character(train$Outlet_Size)
Storetypes <- subset(train, Outlet_Size != "")
spl <- sample.split(Storetypes$Outlet_Size, SplitRatio = 0.8)
train_outlet <- subset(Storetypes, spl == TRUE)
test_outlet <- subset(Storetypes, spl == FALSE)
## Using Random Forest for classification
train_outlet$Outlet_Size <- as.factor(train_outlet$Outlet_Size)
test_outlet$Outlet_Size <- as.factor(test_outlet$Outlet_Size)
## Creating the model
SizeForest <- randomForest(Outlet_Size ~.-Item_Outlet_Sales -Item_Identifier,
                           data =  train_outlet,nodesize = 25, ntree = 100)  
## Predicting on the test set
PredictForest <- predict(SizeForest, newdata = test_outlet)
## Confusion matrix
table(test_outlet$Outlet_Size, PredictForest)
# Classify
train$Outlet_Size <- predict(SizeForest, newdata = train)


# Check rows are all filled? Yes they are
nRows <- nrow(train)
nCompRows <- sum(complete.cases(train))
nCompRows/nRows

# Get final summary
summary(train)

# Do same everything for test
test$Item_Fat_Content <- gsub("LF", "lowfat",test$Item_Fat_Content)
test$Item_Fat_Content <- gsub("low fat", "lowfat",test$Item_Fat_Content)
test$Item_Fat_Content <- gsub("Low Fat", "lowfat",test$Item_Fat_Content)
test$Item_Fat_Content <- gsub("reg", "Regular",test$Item_Fat_Content)
test$Item_Fat_Content <- as.factor(test$Item_Fat_Content)
MeanItem_Weight <- mean(test$Item_Weight[!is.na(test$Item_Weight)])
test$Item_Weight[is.na(test$Item_Weight)] <- MeanItem_Weight
test_temp <- test %>% filter(Item_Visibility != 0)
visibility_model <- lm(Item_Visibility ~ Item_Weight + Item_Fat_Content +
                         Item_Type + Item_MRP +
                         Outlet_Establishment_Year + Outlet_Size +
                         Outlet_Location_Type,
                       data = test_temp)
test$Item_Visibility[test$Item_Visibility == 0] <-
  predict(visibility_model,newdata = test[test$Item_Visibility == 0,])
set.seed(100)
test$Outlet_Size <- as.character(test$Outlet_Size)
Storetypes <- subset(test, Outlet_Size != "")
spl <- sample.split(Storetypes$Outlet_Size, SplitRatio = 0.8)
test_outlet <- subset(Storetypes, spl == TRUE)
test_outlet <- subset(Storetypes, spl == FALSE)
test_outlet$Outlet_Size <- as.factor(test_outlet$Outlet_Size)
test_outlet$Outlet_Size <- as.factor(test_outlet$Outlet_Size)
SizeForest <- randomForest(Outlet_Size ~.-Item_Identifier,
                           data =  test_outlet,nodesize = 25, ntree = 100)  
PredictForest <- predict(SizeForest, newdata = test_outlet)
test$Outlet_Size <- predict(SizeForest, newdata = test)


# Building the model
test_predict <- test
sales_model <- lm(Item_Outlet_Sales ~ Item_Weight + Item_Fat_Content +
                    Item_Visibility + Item_MRP + 
                    Outlet_Establishment_Year + Outlet_Size +
                    Outlet_Location_Type + Outlet_Type,
                  data = train)
summary(sales_model)
sales <- predict(sales_model, newdata = test_predict)

test_pred$Item_Outlet_Sales <- as.vector(sales)

# Rename the predicted sales column
names(test_pred)[12] <- "Item_Outlet_Sales"

answer = test_pred[, c("Item_Identifier", "Outlet_Identifier", "Item_Outlet_Sales")]
write.csv(answer, "C:/Users/User/Downloads/research paper/Result.csv")
