library(shiny)
#library(shinydashboard)


ui <- fluidPage(
  titlePanel("CHIME outcomes model for diabetes and prediabetes"),
  
  
  tabsetPanel(id = "all",
              
            
    tabPanel("Diabetes",  fluid = F, 
             sidebarLayout(
              sidebarPanel(width = 2,
                selectInput("dm_hba1c", "Hba1c (%)",
                            choices = list("6.5" = 6.5,  
                                           "8" = 8),
                            selected = 6.5),
                radioButtons("dm_female", "Gender", 
                             choices = list("Male" = 0, "Female" = 1), 
                             selected = 1),
                selectInput("dm_age", "Age", 
                            choices = list("50" = 50, 
                                           "55" = 55,
                                           "60" = 60,
                                           "65" = 65), 
                            selected = 50),
                selectInput("dm_duration", "Duration of diabetes (years)", 
                            choices = list("0" = 0, 
                                           "5" = 5), 
                            selected = 0),
                selectInput("dm_sbp", "Systolic blood pressure (mmHg)", 
                            choices = list("120" = 120, 
                                           "140" = 140),
                            selected = 120),                    
                selectInput("dm_ldl", "LDL-cholesterol (mmol/L)", 
                            choices = list("1.8" = 1.8,
                                           "3.4" = 3.4), 
                            selected = 1.8),
                selectInput("dm_bmi", "BMI", 
                            choices = list("25" = 25, 
                                           "30" = 30), 
                            selected = 25),
                selectInput("dm_egfr", "EGFR", 
                            choices = list("Normal" = "normal", 
                                           "Moderate-severe" = "moderate-severe"), 
                            selected = "normal"),
                selectInput("dm_smoking", "Smoking", 
                            choices = list("Non-smoker" = "Non-smoker",
                                           "Smoker" = "Yes"), 
                            selected = "Non-smoker"), 
                selectInput("dm_insulin", "Insulin", 
                            choices = list("Yes" = 1, 
                                           "No" = 0), 
                            selected = 0), 
                selectInput("dm_non_insulin", "Non-insulin medication", 
                            choices = list("Yes" = 1, 
                                           "No" = 0), 
                            selected = 0), 
                selectInput("dm_statin", "Statin medication", 
                            choices = list("Yes" = 1, 
                                           "No" = 0), 
                            selected = 0), 
                selectInput("dm_bprx", "Blood pressure medication", 
                            choices = list("Yes" = 1, 
                                           "No" = 0), 
                            selected = 0), 
                h5("The risk is based on the following inputs:"),
                h5("Diastolic blood pressure: 77.62"),
                h5("Triglycerides: 1.61"), 
                h5("HDL - cholesterol: 1.25"), 
                h5("White blood cell count: 2.32"), 
                h5("Hemoglobin: 13.68"), 
                h5("No previous history of complications")
              ),
              mainPanel(uiOutput("dm_plot")))),
                
    
    
    tabPanel("Pre-diabetes", id = "pre_dm", fluid = TRUE, 
             sidebarLayout(
               sidebarPanel(width = 2,
                    selectInput("pre_dm_hba1c", "Hba1c (%)",
                                choices = list("5.7" = 5.7, 
                                               "6.0" = 6.0), 
                                selected = 5.7),
                    radioButtons("pre_dm_female", "Gender", 
                                 choices = list("Male" = 0, "Female" = 1), 
                                 selected = 1),  
                    selectInput("pre_dm_age", "Age", 
                                choices = list("50" = 50, 
                                               "60" = 60, 
                                               "70" = 70), 
                                selected = 50),
                    selectInput("pre_dm_sbp", "Systolic blood pressure (mmHg)", 
                                choices = list("120" = 120, 
                                               "140" = 140),
                                selected = 120),                    
                    selectInput("pre_dm_ldl", "LDL-cholesterol (mmol/L)", 
                                choices = list("1.8" = 1.8,
                                               "3.4" = 3.4), 
                                selected = 1.8),
                    selectInput("pre_dm_bmi", "BMI", 
                                choices = list("25" = 25, 
                                               "30" = 30), 
                                selected = 25),
                    selectInput("pre_dm_egfr", "EGFR", 
                                choices = list("Normal" = "normal", 
                                               "Moderate-severe" = "moderate-severe"), 
                                selected = "normal"),
                    selectInput("pre_dm_smoking", "Smoking", 
                                choices = list("Non-smoker" = "Non-smoker",
                                               "Smoker" = "Yes"), 
                                selected = "Non-smoker"), 
                    selectInput("pre_dm_statin", "Statin medication", 
                                choices = list("Yes" = 1, 
                                               "No" = 0), 
                                selected = 0), 
                    selectInput("pre_dm_bprx", "Blood pressure medication", 
                                choices = list("Yes" = 1, 
                                               "No" = 0), 
                                selected = 0), 
                    h5("The risk is based on the following inputs:"),
                    h5("Diastolic blood pressure: 77.62"),
                    h5("Triglycerides: 1.61"), 
                    h5("HDL - cholesterol: 1.25"), 
                    h5("White blood cell count: 2.32"), 
                    h5("Hemoglobin: 13.68"), 
                    h5("No previous history of complications")
               ),
                   
             mainPanel(fluidRow(uiOutput("pre_dm_plot"))))
             
          ),
    
    
    tabPanel("Risk equations", id = "eq", fluid = TRUE, 
             sidebarLayout(
               sidebarPanel(width = 2,
                            radioButtons("condition", "Outcome", 
                                         choices = list("Amputation" = "Amputation", 
                                                        "Cataract" = "Cataract" , 
                                                        "Death" = "Death", 
                                                        "Diabetes" = "Diabetes", 
                                                        "Heart failure" = "Heart failure", 
                                                        "IHD" = "IHD", 
                                                        "MI" = "MI", 
                                                        "Neuropathy" = "Neuropathy", 
                                                        "PVD" = "PVD", 
                                                        "Renal failure" = "Renal failure", 
                                                        "Retinopathy" = "Retinopathy", 
                                                        "Skin ulcer" = "Skin ulcer", 
                                                        "Stroke" = "Stroke"), 
                                         selected = "Amputation")),
               mainPanel(fluidRow(uiOutput("equation"))), 
    )),
    
    tabPanel("Publication & Dataset", id = "eq", fluid = TRUE, 
             mainPanel(
               
               h4("Chinese Hong Kong Integrated Modelling and Evaluation (CHIME) of diabetes and prediabetes",
               br()),
               
               p(br(),
                 "J Quan, CS Ng", 
                 br(), 
                 "School of Public Health, LKS Faculty of Medicine, The University of Hong Kong", 
                 br(),
                 br()),
               
               p(strong("Citation:"), 
                 br(),
                 strong("Quan J"), ", Ng CS", "Kwok HHY, Zhang A, Yuen YH, Choi CH, Siu SC, Tang SY, Wat NM, Woo J, Eggleston K, Leung GM. (2021)",
                 "Development and validation of the CHIME simulation model to assess lifetime health outcomes of prediabetes and type 2 diabetes in Chinese populations: A modeling study.", em(strong("PLOS Medicine")), "18(6): e1003692.", a("https://doi.org/10.1371/journal.pmed.1003692", href="https://doi.org/10.1371/journal.pmed.1003692")
                 ),
               
               p(br(),
                 strong("Model dataset @ HKU Data Hub"), 
                 br(), 
                 a("CHIME_models.Rdata", href="https://dx.doi.org/10.25442/hku.16864042")),
               
               p(br(),
                 strong("Code @ GitHub"), 
                 br(),
                 a("https://github.com/quan-group/CHIME", href="https://github.com/quan-group/CHIME"))
               
               )
             )
))


