library(shiny)
library(rsconnect)
library(ggplot2)
library(shinyjs)
library(dplyr)
library(tidyverse)
library(superml)
label <- LabelEncoder$new()

#set up the account info
rsconnect::setAccountInfo(name='asyiqin-harmizi',
                          token='F52E35326C61D586AE6895D1A9B98C51',
                          secret='E90MdzQkYRNmczZaFJh0SIyY/jJgBn3Uf9XU1eTz')

ui <- fluidPage(
  titlePanel("Diabetes Risk Predictor"),
  sidebarLayout(
    sidebarPanel(
      
      #get User Input
      sliderInput(inputId="age",label="Enter your Age (Move The Slider):",value=22,min=1,max=90),
      numericInput(inputId="bmi",label="Enter your Body Mass Index (BMI) :",value=23.8,min=12,max=45),
      radioButtons(inputId = "sex",label="Specify your Gender: ",c("Female"="Female","Male"="Male")),
      radioButtons(inputId = "blood_press",label="Have you EVER been told by a doctor, nurse or other health professional that you have high blood pressure?",
                   c("Yes"="Yes","Yes, Prehypertensive" = "Prehypertensive","No"="No")),
      radioButtons(inputId = "high_chol",label="Have you EVER been told by a doctor, nurse or other health professional that your blood cholesterol is high? "
                   ,c("Yes"="Yes","No"="No")),
      radioButtons(inputId = "smoking",label="Have you EVER been smoking? ",c("Yes"="Yes","No"="No")),
      radioButtons(inputId = "stroke",label="Have you EVER get any stroke before? " ,c("Yes"="Yes","No"="No")),
      radioButtons(inputId = "physAct",label="Have you EVER doing physical activity or exercise during the past 30 days other than their regular job " ,
                   c("Has Physical Activity"="Has Physical Activity","No Physical Activity"="No Physical Activity")),
      radioButtons(inputId = "alcohol",label="Are you a heavy drinker?" ,c("Yes"="Yes","No"="No")),
      radioButtons(inputId = "heart",label="Have you EVER get any heart disease? " ,c("Yes"="Yes","No"="No")),
      numericInput(inputId = "physhlth",label="For how many days during the past 30 days was your physical health not good? Enter the number of days:",
                   value=12, min=0,max=30),
      radioButtons(inputId = "diffwalk",label="Do you have serious difficulty walking or climbing stairs? " ,c("Yes"="Yes","No"="No")),
      br(),
      actionButton("enter", label = "Predict")
    ),
    mainPanel(
      tabsetPanel(type="tabs",
                  
                  #Tab 1 - show user input details and comments based on input
                  
                  tabPanel("Survey Details",br(), 
                           h3("Your Input Details: "),
                           h4("Age:"),verbatimTextOutput("age"),
                           h4("Body Weight Index:"),verbatimTextOutput("bmi"),
                           h4("Gender:"),verbatimTextOutput("sex"),
                           h4("Being told by health professionals that you have high blood pressure?"),verbatimTextOutput("blood_press"),
                           h4("been told by health professionals that your blood cholesterol is high?"),verbatimTextOutput("high_chol"),
                           h4("Have you EVER been smoking? "), verbatimTextOutput("smoking"),
                           h4("Have you EVER get any stroke before?"),verbatimTextOutput("stroke"),
                           h4("Have you EVER being active physically?"),verbatimTextOutput("physAct"),
                           h4("Are you a heavy drinker? "),verbatimTextOutput("alcohol"),
                           h4("Have you EVER get any heart disease?"),verbatimTextOutput("heart"), 
                           h4("For how many days during the past 30 days was your physical health not good?"),verbatimTextOutput("physhlth"), 
                           h4("Difficulty walking or climbing stairs?"),verbatimTextOutput("diffwalk"),
                  ),
                  
                  #Tab 2 - provide help / guideline to user when input details
                  
                  tabPanel("Diabetes Description & Documentation",br(),
                           h4("Body Mass Index (BMI): "),
                           h5("How to calculate your BMI?"),
                           p("- Using this formula given:  Weight/((Height)^2)"),
                           p("- Please make sure that your weight is in kilogram (kg) and your height is in metre (m)."),
                           
                           
                           br(),
                           h4("High Blood Pressure: "),
                           h5("Yes"),p("- If you have been checked by the doctor/measure using any blood pressure monitoring tool, and found that you have blood pressure of 140/90 mm Hg or higher"),
                           h5("Yes, Prehypertensive"),p("- If you have been checked by the doctor/measure using any blood pressure monitoring tool, and found that you have blood pressure between 120/80 mm Hg and 139/89 mm Hg."),
                           h5("No"),p("- If you have been checked by the doctor/measure using any blood pressure monitoring tool, and found that you have normal blood pressure of less than 120/80 mm Hg."),
                           
                           br(),
                           h4("Have you EVER being active physically? :"),
                           p("Physical activity refered as all movement including during leisure time, for transport to get to and from places, or as part of a person’s work."),
                           h5("Yes"),p("- More than 75 minutes of moderate physical activity every day."),
                           h5("No"),p("- Less than 75 minutes of physical activity. "),
                           
                           
                           br(),
                           h4("Are you a heavy drinker? "),
                           h5("Yes"),p("- For people who have high alcohol consumption "),
                           h5("No"),p("- For people who have less alcohol consumption or NONE "),
                           
                           br(),
                           h4("For how many days during the past 30 days was your physical health not good? "),
                           p("This part refers to how many days during the past 30 days have your physical health, which includes physical illness and injury, being affected? "),
                           h5("- Maximum Days = 30 days"),
                           p("- Minimum days = 0 days "),
                           
                  ),
                  
                  #Tab 3 - show predicted result and suggestions
                  
                  tabPanel("Result & Suggestion",br(),
                           h3("Predicted Result: "),
                           verbatimTextOutput("predicted"),br(),
                           
                           h3("If you are having the risk of getting Diabetes, What Should You Do? "),br(),
                           h4("1. Get confirmation from the health professionals"),
                           p("- It is better to get confirmed by licensed health practitioners compared to only rely on this findings."),
                           h4("2. Watch Your Food Intake"),
                           p("- Cut sugar and any refined carbohydrates in your diet "),
                           h4("3. Exercise Regularly"),
                           p("- Performing physical activity on a regular basis may help prevent diabetes."),
                           h4("4. Drink Water as Your Primary Beverage "),
                           p("- Drinking water has more benefits compared to drink sugary drinks"),
                           h4("5. Lose Some Weights"),
                           p("- Losing weight are not only remove any visceral fats in your body, but it is also decrease your blood sugar and insulin levels"),
                           h4("6. Quit Smoking and Drinking Alcohol"), 
                           h4("7. Minimize Your Intake of Processed Foods"),
                           h4("8. Watch your food portion"),
                           
                           br(),p("This information and advice is retrieved from 13 Ways to Prevent Type 2 Diabetes by Healthline"),
                           p("Last updated on January 29, 2017 by Franziska Spritzler"),
                           p("Medically reviewed by Angela M. Bell, MD, FACP. "),
                           p("Reference Link: https://www.healthline.com/nutrition/prevent-diabetes")
                  ),
                  
                  #Tab 4 - list of the dataset used
                  
                  tabPanel("Dataset",br(),
                           h3("Diabetes Health Predictor Dataset"),
                           p("- This dataset is based on 2019 Behavioral Risk Factor Surveillance System (BRFSS) Survey Data and Documentation"),
                           p("by the Center of Disease Control and Prevention (CDC)."),
                           dataTableOutput("dataset")
                  ),
                  
                  #Tab 5 - Summary and source of the dataset used
                  
                  tabPanel("Summary",br(),
                           h3("Source of the Dataset: "),
                           p("- This dataset is based on 2019 Behavioral Risk Factor Surveillance System (BRFSS) Survey Data and Documentation"),
                           p("by the Center of Disease Control and Prevention (CDC)."),
                           p("- The size of our dataset: 267902 observations and 21 variables"),
                           
                           br(),
                           h3("Summary of the Dataset: "),
                           verbatimTextOutput("summary")
                  )
                  ))))

# transform user input to align with dataset
transformSEX<-function(sex){
  if(sex=="Female"){
    new="Female"
  }else
    new="Male"
  new
}

transformBP<-function(blood_press){
  if(blood_press=="Yes"){
    new="Yes"
  }else if (blood_press=="Yes, Prehypertensive"){
    new="Prehypertensive"
  }else 
    new="No"
  new
}

transformCHOL<-function(high_chol){
  if(high_chol=="Yes"){
    new="Yes"
  }else 
    new="No"
  new
}

transformSMOKE<-function(smoking){
  if(smoking=="Yes"){
    new="Yes"
  }else 
    new="No"
  new
}

transformSTROKE <- function(stroke){
  if(stroke=="Yes"){
    new="Yes"
  }else 
    new="No"
  new
}

transformPHYSACT<-function(physAct){
  if(physAct=="Has Physical Activity"){
    new="Has Physical Activity"
  }else 
    new="No Physical Activity"
  new
}

transformALCOHOL<-function(alcohol){
  if(alcohol=="Yes"){
    new="Yes"
  }else 
    new="No"
  new
}

transformHEART <- function(heart){
  if(heart=="Yes"){
    new="Yes"
  }else 
    new="No"
  new
}

transformDIFFWALK <- function(diffwalk){
  if(diffwalk=="Yes"){
    new="Yes"
  }else 
    new="No"
  new
}



# give comments on each particular details based on user input (later)

finalResult<-function(value){
  print(format(round(value*100),nsmall=2),"%")
  print("- If the probability of having Diabetes is higher than 50%, it might be a sign that you are having a risk of getting diabetes.")
  print("  Consult with your doctor for health confirmation and advice.")
  print("- If the probability of Healthy is higher than the rest, you have low risk of diabetes")
}

#read the heart data
diabetes.data<- read.csv("diabetes_clean_binary_2019.csv",header=TRUE)
diabetes.subdata <- diabetes.data[, !names(diabetes.data) %in% c("year", "age_category","education", "income","weight",
                                      "MentalHlth","AnyHealthcare", "medicalCost", "GenHlth", "height")]

#Label and factorize diabetes column 
diabetes.subdata$diabetes <- label$fit_transform(diabetes.subdata$diabetes)
diabetes.subdata$diabetes <- as.factor(diabetes.subdata$diabetes)

server <- function(input, output) {
  output$age<- renderText(input$age)
  output$sex<- renderText(input$sex)
  output$bmi<- renderText(input$bmi)
  output$blood_press <- renderText(input$blood_press)
  output$high_chol <- renderText(input$high_chol)
  output$smoke <- renderText(input$smoking)
  output$stroke <- renderText(input$stroke)
  output$physAct <- renderText(input$physAct)
  output$alcohol <- renderText(input$alcohol)
  output$heart <- renderText(input$heart)
  output$physhlth <- renderText(input$physhlth)
  output$diffwalk <- renderText(input$diffwalk)  
  
  output$value <- renderText({check(input$age,
                                    input$bmi,
                                    transformSEX(input$sex),
                                    transformBP(input$blood_press),
                                    transformCHOL(input$high_chol),
                                    transformSMOKE(input$smoking),
                                    transformSTROKE(input$stroke),
                                    transformPHYSACT(input$physAct),
                                    transformALCOHOL(input$alcohol),
                                    transformHEART(input$heart),
                                    input$physhlth,
                                    transformDIFFWALK(input$diffwalk))})
  
  val <- eventReactive(
    input$enter, {
      data.frame(
        age = as.numeric(round(scale(input$age))),
        gender = as.character({transformSEX(input$sex)}),
        bmi = as.numeric(round(scale(input$bmi))),
        blood_pressure = as.factor(label$fit_transform({transformBP(input$blood_press)})),
        high_chol = as.factor(label$fit_transform({transformCHOL(input$high_chol)})),
        smoking = as.factor(label$fit_transform({transformSMOKE(input$smoking)})),
        stroke = as.factor(label$fit_transform({transformSTROKE(input$stroke)})),
        physAct = as.factor(label$fit_transform({transformPHYSACT(input$physAct)})),
        HvyAlchoholconsump = as.factor(label$fit_transform({transformALCOHOL(input$alcohol)})),
        heartDiseaseAttack = as.factor(label$fit_transform({transformHEART(input$heart)})),
        PhysHlth = as.numeric(round(scale(input$physhlth))),
        DiffWalk = as.factor(label$fit_transform({transformDIFFWALK(input$diffwalk)})),
        stringsAsFactors = T
      )}
  )
  
  #prediction based on user input
  output$dataset <- renderDataTable({diabetes.data})
  output$summary <- renderPrint({summary(diabetes.data)})
  
  #load rds file that contains the best model
  model_glm <- glm(diabetes ~ ., data = diabetes.subdata, family = "binomial")
  
  output$predicted <- renderText({finalResult({predict(model_glm, val(), type = "response")%>% tail(1)})})
}

shinyApp(ui = ui, server = server)