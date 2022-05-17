
library(dplyr)
dtbin <- read.csv("E:/Data Science UM/Semester 5 sesi 2122/WIH3001- Data Science Project/Diabetes Dataset/diabetes_clean_binary_2019.csv",
                  header= TRUE, 
                  sep =",")

dtbin <- dtbin[, !names(dtbin) %in% c("year", "age_category","education", "income",
                                     "MentalHlth","AnyHealthcare", "medicalCost", "GenHlth", "height")]
#define Min-Max normalization function
library(BBmisc)
dtbin$bmi <- scale(dtbin$bmi)
dtbin$bmi <- round(dtbin$bmi, 3)
dtbin$PhysHlth <- scale(dtbin$PhysHlth)
dtbin$PhysHlth<- round(dtbin$PhysHlth, 3)
dtbin$age <- scale(dtbin$age)
dtbin$age <- round(dtbin$age, 3)

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
dtbin$smoking <- label$fit_transform(dtbin$smoking)
str(dtbin)

#categorize diabetes column
dtbin$diabetes <- as.factor(dtbin$diabetes)

library(plyr)
count(dtbin, "diabetes")

#Splitting data
split_index <- sort(sample(nrow(dtbin), nrow(dtbin)*0.8))
train_set <- dtbin[split_index,]
test_set <- dtbin[-split_index,]

library(caret)
library(UBL)
#Undersampling
count(train_set, "diabetes")
tr_under <- RandUnderClassif(diabetes~., train_set, C.perc = "balance")
count(tr_under, "diabetes")
#Oversampling
tr_over <- RandOverClassif(diabetes~., train_set, C.perc = "balance")
count(tr_over, "diabetes")

#For Cross validation to 10 fold
tr_control <- trainControl(method = "cv", number = 10, savePredictions = TRUE)

#Model training
fit_under <- train(form = diabetes ~ .,
                   data = tr_under,
                   method = "glm",
                   family = "binomial")
fit_under
confusionMatrix(fit_under, mode = "everything", positive = "1")
confusionMatrix(predicted, actual,
                mode = "everything",
                positive="1")
#Accuracy = 0.7079008, Kappa = 0.4158016


fit_over <- train(form = diabetes ~ .,
                  data = tr_over,
                  method = "glm",
                  family = "binomial" )
fit_over
confusionMatrix(fit_over)
#Accuracy = 0.7087781, Kappa = 0.4175561

fit_all <- train(form = diabetes ~ .,
                 data = train_set,
                 method = "glm",
                 family = "binomial" )
fit_all
confusionMatrix(fit_all)


#Using glm
fit_under2 <- glm(diabetes ~ ., data = tr_under, family = "binomial")
fit_under2

tr_under_pred <- predict(fit_under2, train_set, type = 'response')
tr_under_pred <- ifelse(tr_under_pred >=0.5 , "TRUE", "FALSE")
tr_under_matrix <- table(Prediction = tr_under_pred , Actual = train_set$diabetes)
tr_under_accuracy <- sum(tr_under_pred == train_set$diabetes)/nrow(train_set)*100

test_under_pred <- predict(fit_under2, test_set, type = 'response')
test_under_pred <- ifelse(test_under_pred >=0.5 ,"TRUE", "FALSE")
test_under_matrix <- table(Prediction = test_under_pred , Actual = test_set$diabetes)
test_under_accuracy <- sum(test_under_pred == test_set$diabetes)/nrow(test_set)*100

print(tr_under_matrix)
print(tr_under_accuracy)
print(test_under_matrix)
print(test_under_accuracy)
