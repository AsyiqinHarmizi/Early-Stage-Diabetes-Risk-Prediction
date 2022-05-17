# Data Preparation (labeled)

# get the data from the directory
rbfss_2019 <- read.csv("E:/Data Science UM/Semester 5 sesi 2122/WIH3001- Data Science Project/Diabetes Dataset/LLCP2019.csv",
                       header= TRUE, 
                       sep =",")

# filter columns needed for diabetes analysis
#install.packages("dplyr")
library(dplyr)
db_2019 <- select(rbfss_2019, c('IYEAR', 'SEXVAR','AGE80','WTKG3', 'AGEG5YR', 'EDUCA', 'INCOME2', 'BPHIGH4', 
                                      'TOLDHI2', 'BMI5', 'SMOKE100', 'CVDSTRK3', 'MICHD', 'TOTINDA',
                                      'RFDRHV6', 'HLTHPLN1', 'MEDCOST', 'GENHLTH','MENTHLTH', 'PHYSHLTH',
                                      'DIFFWALK', 'DIABETE4'))
str(db_2019)
colnames(db_2019) <- c('year','gender','age','weight','age_category','education', 'income',
                       'blood_pressure','high_chol', 'bmi', 'smoking','stroke', 'heartDiseaseAttack',
                       'physAct','HvyAlchoholconsump', 'AnyHealthcare', 'medicalCost','GenHlth',
                       'MentalHlth', 'PhysHlth', 'DiffWalk', 'diabetes')

#fill missing values in numerical column
#remove rows that has null values in categorical columns
library(tidyr)
db_2019$bmi[is.na(db_2019$bmi)] <- mean(db_2019$bmi, na.rm = TRUE)
db_2019$weight[is.na(db_2019$weight)] <- mean(db_2019$weight, na.rm = TRUE)
db_2019 <- db_2019%>% drop_na()

db <-db_2019

####### === Diabetes and Other Related Disease Section  === #######
#Diabetes - Replace values to be more suitable for ML algorithms
#Change values to ordinal. 0 is for no diabetes, 
#                          1 is for pre-diabetes or borderline diabetes,
#                          2 is for yes diabetes
                       
# Remove all 7 (don't knows)
# Remove all 9 (refused)
db["diabetes"][db["diabetes"] == 3] <- "Healthy"
db["diabetes"][db["diabetes"] == 2] <- "Gestational Diabetes"
db["diabetes"][db["diabetes"] == 1] <- "Diabetes"
db["diabetes"][db["diabetes"] == 4] <- "Prediabetes"
db <- filter(db, (diabetes!=7) & (diabetes!=9))
unique(db$diabetes)

#High Blood Pressure - Replace values to only 2 options:
#     0 for no blood pressure
#     1 for yes high blood pressure
#     2 for during pregnancy only
#     Remove all 7 (don't knows)
#     Remove all 9 (refused)
db$blood_pressure[db["blood_pressure"] == 1] <- "Yes"
db$blood_pressure[db["blood_pressure"] == 2] <- "Yes, During Pregnancy"
db$blood_pressure[db["blood_pressure"] == 3] <- "No"
db$blood_pressure[db["blood_pressure"] == 4] <- "Prehypertensive"
db <- filter(db, (blood_pressure!=7) & (blood_pressure!=9))

#High Cholesterol - replace value 2 to 0, since 2 represents No High Cholesterol
# Remove all 9 (refused)
db$high_chol[db["high_chol"] == 1] <- "Yes"
db$high_chol[db["high_chol"] == 2] <- "No"
db <- filter(db, (high_chol!=7) & (high_chol!=9))

#Stroke - Replace value 2 to 0 because it is a No
# Remove all 7 (don't knows) and 9 (refused)
db$stroke[db["stroke"] == 1] <- "Yes"
db$stroke[db["stroke"] == 2] <- "No"
db <- filter(db, (stroke!=7) & (stroke!=9))

#Heart Disease or Attack - Change 2 to 0 because this means did not have 
#                          coronary heart disease (CHD) or myocardial infarction (MI)
db$heartDiseaseAttack[db["heartDiseaseAttack"] == 1] <- "Yes"
db$heartDiseaseAttack[db["heartDiseaseAttack"] == 2] <- "No"


####### ===  LifeStyle Section  === #######

#Heavy drinker - Replace values
# Change 1 to 0 (1 was no for heavy drinking). change all 2 to 1 (2 was yes for heavy drinking)
# remove all don't knows and missing 9

db["HvyAlchoholconsump"][db["HvyAlchoholconsump"] == 1] <- "No"
db["HvyAlchoholconsump"][db["HvyAlchoholconsump"] == 2] <- "Yes"
db <- filter(db, (HvyAlchoholconsump!=9))

#Physical Activity - change the values
#    1 for yes physical activity
#    change 2 to 0 for no physical activity
# Remove all 9 (for don't know/ refused)
db$physAct[db["physAct"] == 1] <- "Have Physical Activity"
db$physAct[db["physAct"] == 2] <- "No Physical Activity"
db <- filter(db, (physAct!=9))

#Smoking - Replace value 2 to 0 because it is a No
# Remove all 7 (don't knows) and 9 (refused)
db$smoking[db["smoking"] == 1] <- "Yes"
db$smoking[db["smoking"] == 2] <- "No"
db <- filter(db, (smoking!=7) & (smoking!=9))


####### === General Health care and Mental Health Section === #######

#Change values for AnyHealthcare and medicalCost
# 1 is yes, change 2 to 0 because it is No health care access
# remove 7 and 9 for don't know or refused
#AnyHealthcare
db["AnyHealthcare"][db["AnyHealthcare"] == 1] <- "Have Healthcare Access"
db["AnyHealthcare"][db["AnyHealthcare"] == 2] <- "No Healthcare Access"
db <- filter(db, (AnyHealthcare!=7) & (AnyHealthcare!=9))
#medicalCost
db["medicalCost"][db["medicalCost"] == 1] <- "Yes"
db["medicalCost"][db["medicalCost"] == 2] <- "No"
db <- filter(db, (medicalCost!=7) & (medicalCost!=9))

# Remove all 7 (don't know) and 9 (refused) in general health (GenHlth)
db["GenHlth"][db["GenHlth"] == 1] <- "Excellent"
db["GenHlth"][db["GenHlth"] == 2] <- "Very Good"
db["GenHlth"][db["GenHlth"] == 3] <- "Good"
db["GenHlth"][db["GenHlth"] == 4] <- "Fair"
db["GenHlth"][db["GenHlth"] == 5] <- "Poor"
db <- filter(db, (GenHlth!=7) & (GenHlth!=9))

# Change 88 to 0 for no bad mental health days
# Remove 77 and 99 don't know not sure and refused (MentalHlth and PhysHlth)
#MentalHlth
db["MentalHlth"][db["MentalHlth"] == 88] <- 0
db <- filter(db, (MentalHlth!=77) & (MentalHlth!=99))
#PhysHlth
db["PhysHlth"][db["PhysHlth"] == 88] <- 0
db <- filter(db, (PhysHlth!=77) & (PhysHlth!=99))

# DiffWalk - Change values
# 2 -> 0 (for no)
# Remove all 7 and 9 for don't know not sure and refused
db["DiffWalk"][db["DiffWalk"] == 1] <- "Yes"
db["DiffWalk"][db["DiffWalk"] == 2] <- "No"
db <- filter(db, (DiffWalk!=7) & (DiffWalk!=9))


####### === General Information Section  === #######
# Age - remove 14 it is don't know or missing
db["age_category"][db["age_category"] == 1] <- "18-24"
db["age_category"][db["age_category"] == 2] <- "25-29"
db["age_category"][db["age_category"] == 3] <- "30-34"
db["age_category"][db["age_category"] == 4] <- "35-39"
db["age_category"][db["age_category"] == 5] <- "40-44"
db["age_category"][db["age_category"] == 6] <- "45-49"
db["age_category"][db["age_category"] == 7] <- "50-54"
db["age_category"][db["age_category"] == 8] <- "55-59"
db["age_category"][db["age_category"] == 9] <- "60-64"
db["age_category"][db["age_category"] == 10] <- "65-69"
db["age_category"][db["age_category"] == 11] <- "70-74"
db["age_category"][db["age_category"] == 12] <- "75-79"
db["age_category"][db["age_category"] == 13] <- ">80"
db <- filter(db, (age_category!=14))

# Education - remove 9 for refused
db["education"][db["education"] == 1] <- "Never Attended School"
db["education"][db["education"] == 2] <- "Elementary Level"
db["education"][db["education"] == 3] <- "Some High School"
db["education"][db["education"] == 4] <- "High School Graduate"
db["education"][db["education"] == 5] <- "College student"
db["education"][db["education"] == 6] <- "College Graduate"
db <- filter(db, (education!=9))

# Income - remove 77 an 99 for don't know and refused
db["income"][db["income"] == 1] <- "< 10,000"
db["income"][db["income"] == 2] <- "10,000 - 15,000"
db["income"][db["income"] == 3] <- "15,000 - 20,000"
db["income"][db["income"] == 4] <- "20,000 - 25,000"
db["income"][db["income"] == 5] <- "25,000 - 35,000"
db["income"][db["income"] == 6] <- "35,000 - 50,000"
db["income"][db["income"] == 7] <- "50,000 - 75,000"
db["income"][db["income"] == 8] <- "> 75,000"
db <- filter(db, (income!=77) & (income!=99))


#BMI, weight and height - Move 2 decimal places to the left
str(db)
db$bmi <- as.numeric(db$bmi)/100
db$weight <- as.numeric(db$weight)/100
#Year - replace values in year column
db$year[db$year == "b'2019'"] <- '2019'
db$year[db$year == "b'2020'"] <- '2020'
# Removing any outliers in bmi
db <- filter(db, (bmi <= 45))

#Gender - replace values
db$gender[db$gender == 1] <- "Male"
db$gender[db$gender == 2] <- "Female"

summary(db)
str(db)

#Save to CSV
write.csv(db,
          "E:/Data Science UM/Semester 5 sesi 2122/WIH3001- Data Science Project/Diabetes Dataset/diabetes_clean_2019.csv",
          row.names = FALSE)