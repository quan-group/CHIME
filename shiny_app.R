
library(tidyverse)
library(shiny)


ui <- fluidPage(
  titlePanel("CHIME model: Chinese Hong Kong Integrated Modelling and Evaluation of diabetes and prediabetes"),
  tabsetPanel(id = "all",
    tabPanel("Diabetes",  fluid = TRUE, 
             sidebarLayout(
              sidebarPanel(
                selectInput("dm_hba1c", "Hba1c (%)",
                            choices = list("6.5" = 6.5, 
                                           "7.0" = 7.0, 
                                           "7.5" = 7.5, 
                                           "8.0" = 8.0, 
                                           "8.5" = 8.5),
                            selected = 7.0),
                radioButtons("dm_female", "Gender", 
                             choices = list("Male" = 0, "Female" = 1), 
                             selected = 1),
                selectInput("dm_age", "Age", 
                            choices = list("40-49" = 45, 
                                           "50-59" = 55, 
                                           "60-69" = 65, 
                                           "70-79" = 75), 
                            selected = 55),
                selectInput("dm_duration", "Duration of diabetes (years)", 
                            choices = list("0" = 0, 
                                           "1" = 1, 
                                           "3" = 3, 
                                           "5" = 5,
                                           "10" = 10), 
                            selected = 0),
                selectInput("dm_sbp", "Systolic blood pressure (mmHg)", 
                            choices = list("120" = 120, 
                                           "130" = 130, 
                                           "140" = 140, 
                                           "150" = 150),
                            selected = 130),                    
                selectInput("dm_ldl", "LDL-cholesterol (mmol/L)", 
                            choices = list("1.8" = 1.8, 
                                           "2.6" = 2.6,
                                           "3.4" = 3.4), 
                            selected = 2.6),
                selectInput("dm_bmi", "BMI", 
                            choices = list("Normal" = 20.75, 
                                           "Overweight" = 24, 
                                           "Obese" = 26), 
                            selected = 24),
                selectInput("dm_survival_time", "Time horizon (years)", 
                            choices = list("5" = 5, 
                                           "10" = 10), selected = 5)
              ),
              mainPanel(plotOutput("myPlot_dm")))) ,
                
                
                
    
    tabPanel("Pre-diabetes", id = "pre_dm", fluid = TRUE, 
             sidebarLayout(
               sidebarPanel(
                    selectInput("pre_dm_hba1c", "Hba1c (%)",
                                choices = list("5.7" = 5.7, 
                                               "6.0" = 6.0, 
                                               "6.4" = 6.4), 
                                selected = 5.7),
                    radioButtons("pre_dm_female", "Gender", 
                                 choices = list("Male" = 0, "Female" = 1), 
                                 selected = 1),
                    selectInput("pre_dm_age", "Age", 
                                choices = list("40-49" = 45, 
                                               "50-59" = 55, 
                                               "60-69" = 65, 
                                               "70-79" = 75), 
                                selected = 55),
                    selectInput("pre_dm_sbp", "Systolic blood pressure (mmHg)", 
                                choices = list("120" = 120, 
                                               "130" = 130, 
                                               "140" = 140, 
                                               "150" = 150),
                                selected = 130),                    
                    selectInput("pre_dm_ldl", "LDL-cholesterol (mmol/L)", 
                                choices = list("1.8" = 1.8, 
                                               "2.6" = 2.6,
                                               "3.4" = 3.4), 
                                selected = 2.6),
                    selectInput("pre_dm_bmi", "BMI", 
                                choices = list("Normal" = 20.75, 
                                               "Overweight" = 24, 
                                               "Obese" = 26), 
                                selected = 24),
                    selectInput("pre_dm_survival_time", "Time horizon (years)", 
                                choices = list("5" = 5, 
                                               "10" = 10), selected = 5)
                ),
             mainPanel(fluidRow(plotOutput("myPlot_pre_dm")))) 
               
               
      )



))


server = function(input, output){
  dm_data <- readRDS("dm_data.rds")
  pre_dm_data <- readRDS("pre_dm_data.rds")
    
  
   dm1 <- reactive({
    df <- dm_data[which(dm_data$hba1c %in% input$dm_hba1c & 
                          dm_data$age %in% input$dm_age & 
                          dm_data$duration %in% input$dm_duration & 
                          dm_data$bmi %in% input$dm_bmi & 
                          dm_data$sbp %in% input$dm_sbp & 
                          dm_data$ldl %in% input$dm_ldl & 
                          dm_data$survival_time %in% input$dm_survival_time & 
                          dm_data$female %in% input$dm_female), ] 
    
    return(df)
   })
   
   draw_chart_dm <- function(df){
     df <- pivot_longer(df, c("Ischaemic heart disease", "Haemodialysis", "Heart failure", "Peripheral vascular disease", "Renal failure", "Neuropathy", "Retinopathy", "Death", "Amputation", 
                              "Cataract", "Myocardial infarction", "Skin ulcer", "Stroke"))
    
    plot1 <-  ggplot(df, aes(y = value, x = name, fill = name)) + geom_bar(position="dodge", stat="identity") + coord_flip() + scale_fill_brewer(palette = "BuPu") +
       ylab("Outcomes per 10,000") + xlab("") + theme_bw() + 
     theme(text = element_text(size=20)) + theme(legend.title = element_blank())
    
    plot1
     
    
   }
  
   draw_chart_pre_dm <- function(df){
     df <- pivot_longer(df, c("Ischaemic heart disease", "Haemodialysis", "Heart failure", "Peripheral vascular disease", "Renal failure", "Neuropathy", "Retinopathy", "Death", "Amputation", 
                              "Cataract", "Myocardial infarction", "Skin ulcer", "Stroke", "Diabetes"))
     
      plot1 <- ggplot(df, aes(y = value, x = name, fill = name)) + geom_bar(position="dodge", stat="identity") + coord_flip() + scale_fill_brewer(palette = "BuPu") + 
        ylab("Outcomes per 10,000") + xlab("") + theme_bw() + theme(legend.title = element_blank()) + theme(text = element_text(size=20))
      
      plot1
    
    }
  
  
    pre_dm1 <- reactive({
      df <- pre_dm_data[which(pre_dm_data$hba1c %in% input$pre_dm_hba1c & 
                                pre_dm_data$age %in% input$pre_dm_age & 
                                pre_dm_data$bmi %in% input$pre_dm_bmi & 
                                pre_dm_data$sbp %in% input$pre_dm_sbp & 
                                pre_dm_data$ldl %in% input$pre_dm_ldl & 
                                pre_dm_data$survival_time %in% input$pre_dm_survival_time & 
                                pre_dm_data$female %in% input$pre_dm_female) ,] 
    
    
    return(df)
    })
      
    
  output$myPlot_dm = renderPlot({
    data <- dm1()
    draw_chart_dm(data)
  })
  
  output$myPlot_pre_dm = renderPlot({
    data <- pre_dm1()
    draw_chart_pre_dm(data)
  })
  
  }



shinyApp(ui = ui, server = server)

