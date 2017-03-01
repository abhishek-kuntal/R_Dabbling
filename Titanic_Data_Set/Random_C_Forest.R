#cleaned code from the console inputs trying to predict on the Titanic Dataset for Survival

setwd("~/Projects/R_Dabbling/Titanic_Data_Set")
train <- read_csv("~/Projects/R_Dabbling/Titanic_Data_Set/train.csv")
test <- read_csv("~/Projects/R_Dabbling/Titanic_Data_Set/test.csv")

#Check the dataset
train
#load Library

library(randomForest)
library(party)
library(rpart)

#Cleaning data for random forest followed DataCamp tutorial for this

all_data$Embarked[c(62, 830)] <- "S"
all_data$Embarked <- factor(all_data$Embarked)
all_data$Fare[1044] <- median(all_data$Fare, na.rm = TRUE)
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size, data = all_data[!is.na(all_data$Age),], method = "anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),])

#splitting the combined dataset to get the new trainign and testing data

train1 <- all_data[1:891,]
test1 <- all_data[892:1309,]

#set seed

set.seed(1111)

# random forests
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + family_size, data = train1, importance = TRUE, ntree = 3000)
my_prediction <- predict(my_forest, test1)
my_solution <- data.frame(PassengerID = test$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "Solution1.csv", row.names = FALSE)

#CI trees

ciforest <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + family_size, data = train1, controls = cforest_unbiased(ntree=1000))
ciforest2 <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + family_size, data = train1, controls = cforest_unbiased(ntree=2000))
ciforest4 <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + family_size, data = train1, controls = cforest_unbiased(ntree=2000, mtry=3))

Prediction1 <- predict(ciforest, test1, OOB = TRUE, type = "response")
Prediction2 <- predict(ciforest2, test1, OOB = TRUE, type = "response")
Prediction3 <- predict(ciforest4, test1, OOB = TRUE, type = "response")


my_solution1 <- data.frame(PassengerID = test$PassengerId, Survived = Prediction1)
my_solution2 <- data.frame(PassengerID = test$PassengerId, Survived = Prediction2)
my_solution3 <- data.frame(PassengerID = test$PassengerId, Survived = Prediction3)

write.csv(my_solution1, file = "Solution2.csv", row.names = FALSE)
write.csv(my_solution2, file = "Solution3.csv", row.names = FALSE)
write.csv(my_solution3, file = "Solution4.csv", row.names = FALSE)
