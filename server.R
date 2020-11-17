library(ggplot2)
library(knitr)


server <- function(input, output){
  
  load("dm_id.RData")
  load("pre_dm_id.RData")
  dm_patients <- readRDS("dm_patients.rds")
  pre_dm_patients <- readRDS("pre_dm_patients.rds")
  
  
   dm1 <- reactive({
     id <- as.numeric(rownames(dm_id[dm_id$hba1c %in% input$dm_hba1c & 
                                       dm_id$age %in% input$dm_age  &
                                       dm_id$duration %in% input$dm_duration & 
                                       dm_id$bmi %in% input$dm_bmi & 
                                       dm_id$sbp %in% input$dm_sbp & 
                                       dm_id$ldl %in% input$dm_ldl & 
                                       dm_id$female %in% input$dm_female &
                                       dm_id$egfr %in% input$dm_egfr &
                                       dm_id$smoking %in% input$dm_smoking &
                                       dm_id$insulin %in% input$dm_insulin &
                                       dm_id$non_insulin %in% input$dm_non_insulin &
                                       dm_id$statin %in% input$dm_statin &
                                       dm_id$bprx %in% input$dm_bprx,]))
     
     df <- dm_patients[[id]][-1]
     for (i in 1:12){
       df[[i]]$condition <- names(df[i])
     }
     df <- do.call(rbind, df)
     df$year <- rep(0:40, 12)
     
     df
   })
   
   pre_dm1 <- reactive({
     pre_id <- as.numeric(rownames(pre_dm_id[pre_dm_id$hba1c %in% input$pre_dm_hba1c & 
                                           pre_dm_id$age %in% input$pre_dm_age  &
                                           pre_dm_id$bmi %in% input$pre_dm_bmi & 
                                           pre_dm_id$sbp %in% input$pre_dm_sbp & 
                                           pre_dm_id$ldl %in% input$pre_dm_ldl & 
                                           pre_dm_id$female %in% input$pre_dm_female &
                                           pre_dm_id$egfr %in% input$pre_dm_egfr &
                                           pre_dm_id$smoking %in% input$pre_dm_smoking &
                                           pre_dm_id$statin %in% input$pre_dm_statin &
                                           pre_dm_id$bprx %in% input$pre_dm_bprx, ]))
     
     pre_df <- pre_dm_patients[[pre_id]][-1]
     for (i in 1:length(pre_df)){
       pre_df[[i]]$condition <- names(pre_df[i])
     }
     pre_df <- do.call(rbind, pre_df)
     pre_df$year <- rep(0:40, 13)
     pre_df
     
   }    )
   
   
   draw_chart_dm <- function(df){
     df <- dm1()
     plot1 <- ggplot(df, aes(x = year, y = prob)) + geom_line() + geom_ribbon(aes(ymin = upper, ymax = lower, fill = condition), alpha = 0.2)+ facet_wrap(. ~ condition) + 
       theme_bw()  + xlab("Years") + ylab("Risk (%)") + theme(legend.position = "none") 
    
    
    plot1
     
    return(plot1)
   }
   draw_chart_pre_dm <- function(df){
     df <- pre_dm1()
     plot1 <- ggplot(df, aes(x = year, y = prob)) + geom_line() + geom_ribbon(aes(ymin = upper, ymax = lower, fill = condition), alpha = 0.2)+ facet_wrap(. ~ condition) + 
       theme_bw()  + xlab("Years") + ylab("Risk (%)") + theme(legend.position = "none") 
     
     
     plot1
     
     return(plot1)
   }
   

    
  output$myPlot_dm <- renderPlot({
    df <- dm1()
    ggplot(df, aes(x = year, y = prob)) + geom_line() + geom_ribbon(aes(ymin = upper, ymax = lower, fill = condition), alpha = 0.2)+ facet_wrap(. ~ condition, scales = "free_y") + 
      theme_bw()  + xlab("Years") + ylab("Risk (%)") + theme(legend.position = "none") 
    
  })
   
  output$dm_plot <- renderUI({
    tabsetPanel(
      tabPanel("", 
                tags$head(tags$style(type = "text/css", "#myPlot_dm {height:85vh !important;}")),
                plotOutput("myPlot_dm")
))
  })
  
  output$myPlot_pre_dm <- renderPlot({
    df <- pre_dm1()
    ggplot(df, aes(x = year, y = prob)) + geom_line() + geom_ribbon(aes(ymin = upper, ymax = lower, fill = condition), alpha = 0.2)+ facet_wrap(. ~ condition, scales = "free_y") + 
      theme_bw()  + xlab("Years") + ylab("Risk (%)") + theme(legend.position = "none") 
    
  })
  
  output$pre_dm_plot <- renderUI({
    tabsetPanel(
      tabPanel("", 
               tags$head(tags$style(type = "text/css", "#myPlot_pre_dm {height:85vh !important;}")),
               plotOutput("myPlot_pre_dm")
      ))
  })
  
  output$equation <-  renderUI({
    if (input$condition == "Amputation"){
      withMathJax(includeMarkdown("amputation.Rmd"))
    }
    else if (input$condition == "Death"){
      withMathJax(includeMarkdown("death.Rmd"))
    }
    else if (input$condition == "MI"){
      withMathJax(includeMarkdown("MI.Rmd"))
    }
    else if (input$condition == "IHD"){
      withMathJax(includeMarkdown("IHD.Rmd"))
    }
    else if (input$condition == "Heart failure"){
      withMathJax(includeMarkdown("heart_failure.Rmd"))
    }
    else if (input$condition == "Stroke"){
      withMathJax(includeMarkdown("stroke.Rmd"))
    }
    else if (input$condition == "PVD"){
      withMathJax(includeMarkdown("pvd.Rmd"))
    }
    else if (input$condition == "Neuropathy"){
      withMathJax(includeMarkdown("neuropathy.Rmd"))
    }
    else if (input$condition == "Skin ulcer"){
      withMathJax(includeMarkdown("ulcer.Rmd"))
    }
    else if (input$condition == "Renal failure"){
      withMathJax(includeMarkdown("renal_failure.Rmd"))
    }
    else if (input$condition == "Cataract"){
      withMathJax(includeMarkdown("cataract.Rmd"))
    }
    else if (input$condition == "Retinopathy"){
      withMathJax(includeMarkdown("retinopathy.Rmd"))
    }
    else if (input$condition == "Diabetes"){
      withMathJax(includeMarkdown("diabetes.Rmd"))
    }
  })
 
  
  }



