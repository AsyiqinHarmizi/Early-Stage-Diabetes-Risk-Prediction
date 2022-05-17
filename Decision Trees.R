
library(dplyr)
dtbin <- read.csv("E:/Data Science UM/Semester 5 sesi 2122/WIH3001- Data Science Project/Diabetes Dataset/diabetes_clean_binary_2019.csv",
                  header= TRUE, 
                  sep =",")
dtbin <- dtbin[, !names(dtbin) %in% c("year", "age_category",
                                      "education", "income",
                                      "smoking", "MentalHlth",
                                      "AnyHealthcare", "medicalCost", "GenHlth"
                                       )]

#Using label encoder to encode categorical data
library(superml)
label <- LabelEncoder$new()
dtbin$gender <- label$fit_transform(dtbin$gender)
dtbin$blood_pressure <- label$fit_transform(dtbin$blood_pressure)
dtbin$high_chol <- label$fit_transform(dtbin$high_chol)
dtbin$DiffWalk <- label$fit_transform(dtbin$DiffWalk)
dtbin$stroke <- label$fit_transform(dtbin$stroke)
dtbin$heartDiseaseAttack <- label$fit_transform(dtbin$heartDiseaseAttack)
dtbin$physAct <- label$fit_transform(dtbin$physAct)
dtbin$HvyAlchoholconsump <- label$fit_transform(dtbin$HvyAlchoholconsump)

#Set as categorical data
dtbin$gender <- as.factor(dtbin$gender)
dtbin$blood_pressure <- as.factor(dtbin$blood_pressure)
dtbin$high_chol <- as.factor(dtbin$high_chol)
dtbin$DiffWalk <- as.factor(dtbin$DiffWalk)
dtbin$diabetes <- as.factor(dtbin$diabetes)
dtbin$stroke <- as.factor(dtbin$stroke)
dtbin$heartDiseaseAttack <- as.factor(dtbin$heartDiseaseAttack)
dtbin$physAct <- as.factor(dtbin$physAct)
dtbin$HvyAlchoholconsump <- as.factor(dtbin$HvyAlchoholconsump)
str(dtbin)

#define Min-Max normalization function
library(BBmisc)
dtbin$bmi <- scale(dtbin$bmi)
dtbin$bmi <- round(dtbin$bmi, 3)
dtbin$PhysHlth <- scale(dtbin$PhysHlth)
dtbin$PhysHlth<- round(dtbin$PhysHlth, 3)
dtbin$age <- scale(dtbin$age)
dtbin$age <- round(dtbin$age, 3)

library(plyr)
count(dtbin, "diabetes")

#Splitting data
split_index <- sort(sample(nrow(dtbin), nrow(dtbin)*0.8))
train_set <- dtbin[split_index,]
test_set <- dtbin[-split_index,]

library(caret)
library(UBL)
#Undersampling
tr_under <- RandUnderClassif(diabetes~., train_set, C.perc = "balance")
count(tr_under, "diabetes")
tr_over <- RandOverClassif(diabetes~., train_set, C.perc = "balance")
count(tr_over, "diabetes")

library(rpart)
library(rpart.plot)

#Function for evaluation
accuracy_tune <- function(fit) {
  predict_tune <- predict(fit, test_set, type = 'class')
  table_mat <- table(test_set$diabetes, predict_tune)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
  confusionMatrix(predict_tune,test_set$diabetes)
}

# Create the tree (for undersampled data)
output.tree <- rpart(diabetes ~ ., data = tr_under, method = 'class')
# Plot the tree
rpart.plot(output.tree, extra = 106)
accuracy_tune(output.tree)

# Create the tree (for oversampled data)
output.tree2 <- rpart(diabetes ~ ., data = tr_over, method = 'class')
# Plot the tree
rpart.plot(output.tree2, extra = 106)
accuracy_tune(output.tree2)

output.tree3 <- rpart(diabetes ~ ., data = train_set, method = 'class')
# Plot the tree
rpart.plot(output.tree3, extra = 106)
accuracy_tune(output.tree3)



#Hyperparameter tuning (undersampled data)
control <- rpart.control(minsplit = 2, minbucket = 3,
                         maxdepth = 12, cp = 0.02, xval = 10)

tune_fit_under <- rpart(diabetes~., data = tr_under, method = 'class', control = control)
rpart.plot(tune_fit_under)
accuracy_tune(tune_fit_under)

#Hyperparameter tuning (oversampled data)
tune_fit_over <- rpart(diabetes~., data = tr_over, method = 'class', control = control)
rpart.plot(tune_fit_over)
accuracy_tune(tune_fit_over)

#Using caret train
fit.control <- trainControl(method = "repeatedcv", number = 5, repeats = 10)
#Undersampled data
tree_under <- train(diabetes~., data = tr_under, method = "rpart", trControl = fit.control, trace = FALSE)
tree_under
confusionMatrix(tree_under)

#Oversampled data
tree_over <- train(diabetes~., data = tr_over, method = "rpart", trControl = fit.control, trace = FALSE)
tree_over
confusionMatrix(tree_over)

#Overall data
tree_all <- train(diabetes~., data = dtbin, method = "rpart", trControl = fit.control, trace = FALSE)
tree_all
confusionMatrix(tree_all)









