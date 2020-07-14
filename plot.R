  #Load results as df:
  
#  listStudy: vector with study names (arms in trials)
#  listOutcome: vector with outcome names
#  listTrial: vector with trial names 
  
  
    df2 <- df %>%
      filter(study_name %in% listStudy & Outcome %in% listOutcome & trial %in% listTrial) 
    
    
    # format(df5$year, "%Y")
    # Visualization
    
    rss <- sum((df2$Predicted - df2$Observed)^2)
    tss <- sum((df2$Observed - mean(df2$Observed))^2)
    m <- lm(df2$Observed ~ df2$Predicted)
    rsq <- summary(m)$r.squared
    rmspe <- sqrt(sum((df2$Predicted - df2$Observed)^2)/nrow(df2))
    
    my_plot <-  ggplot(df2, aes(x = Observed, y = Predicted, text = Outcome, colour = decade, 
                                label = trial)) + #colour = study_name, text = Outcome),) + 
      geom_point(size = 1.5) + 
      geom_abline(intercept = 0, slope = 1, color = "blue") +
      #geom_smooth(method= "lm", color = "red") +
      annotate('text', x = 8, y = 95, label = paste0(sprintf("<i>%s</i>", "R"), "<sup>2</sup>", "= ", format(rsq, digits = 3))) +
      annotate('text', x = 12, y = 90, label = paste0("RMSPE", "= ", format(rmspe, digits = 3), "%")) +
      #geom_text_repel(aes(label = Outcome, color =Outcome), size = 3) +
      xlim(0, 100) +
      xlab("Observed") +
      ylim(0, 100) +
      theme_bw() + theme(legend.title = element_blank())
      
    
#interactive plot

    ggplotly(my_plot, tooltip = c("label", "Outcome"), height = 500, width = 650)
