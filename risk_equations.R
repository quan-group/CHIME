#load data as complete


nums <- unlist(lapply(complete, is.numeric))  
non_linear <- names(complete[, nums])
non_linear <- non_linear[4:14]
nonlinear_terms <- paste0("rcs(", non_linear, ", 3)")
full_model <- c(nonlinear_terms, "female", "status", "smoking", "egfr", 
                "dm_insulin", "dm_non_insulin", "bprx", "statin", 
                "history_amputation", "history_cataract", "history_ihd", 
                "history_heart_failure", 
                "history_neuropathy", 
                "history_retinopathy_all", 
                "history_haemodialysis", "history_mi", 
                "history_pvd", 
                "history_stroke", "history_ulcer_skin", "history_af")


FUN.episodic_outcomes <- function(outcome, rm_var){
  k <- substitute(outcome)
  #h <- substitute(history)
  
  outcome <- complete %>%
    mutate(time = case_when(is.na(eval(k)) ~ as.numeric(end_date - entry_date), 
                            TRUE ~ as.numeric(eval(k) - entry_date)),
           event = case_when(is.na(eval(k)) ~ 0,
                             TRUE ~ 1))
  
  outcome$time[outcome$time == 0] <- NA
  outcome <- outcome[!is.na(outcome$time),]
  outcome$time <- as.numeric(outcome$time)
  
  outcome <- outcome[outcome$time >= 90, ] # minimum follow-up/event >=90 days (see RCT)
  print(table(outcome$event))
  
  outcome$survival <- Surv(outcome$time, outcome$event)
  
  full_model <- full_model[!full_model %in% rm_var] 
  
  fit_weibull <- psm(as.formula(paste0("survival ~ ", paste(full_model, collapse = " + "))), 
                    data=outcome, model = T, x = T, y = T, dist = "weibull", control = survreg.control(maxiter = 200))
  
  fit_exp <- psm(as.formula(paste0("survival ~ ", paste(full_model, collapse = " + "))),
                 data=outcome, model = T, x = T, y = T, dist = "exponential", control = survreg.control(maxiter = 200))
  
  fit_loglog <- psm(as.formula(paste0("survival ~ ", paste(full_model, collapse = " + "))),
                    data=outcome, model = T, x = T, y = T, dist = "loglogistic", control = survreg.control(maxiter = 200))
  
  fit_lognormal <- psm(as.formula(paste0("survival ~ ", paste(full_model, collapse = " + "))),
                       data=outcome, model = T, x = T, y = T, dist = "lognormal", control = survreg.control(maxiter = 200))
  
  model_stats <- data.frame(distr = c("weibull", 
                                      "exp", "loglog", "lognormal"), 
                            loglik_full = NA, 
                            AIC_full = NA, 
                            AIC_final = NA,
                            c_stat = NA, 
                            c_lower = NA, 
                            c_upper = NA,
                            brier5 = NA, 
                            brier5_lower = NA, 
                            brier5_upper = NA, 
                            brier10 = NA, 
                            brier10_lower = NA, 
                            brier10_upper = NA)
  model_stats$loglik_full <- c(mean(fit_weibull$loglik[1], fit_weibull$loglik[2]),
                               mean(fit_exp$loglik[1], fit_exp$loglik[2]), 
                               mean(fit_loglog$loglik[1], fit_loglog$loglik[2]), 
                               mean(fit_lognormal$loglik[1], fit_lognormal$loglik[2]))
  
  model_stats$AIC_full <- c(AIC(fit_weibull), 
                            AIC(fit_exp), 
                            AIC(fit_loglog), 
                            AIC(fit_lognormal))
  
  full_models <- list(fit_weibull, 
                      fit_exp, 
                      fit_loglog, 
                      fit_lognormal)
  
  names(full_models) <- c("weibull", 
                          "exp", "loglog", "lognormal")
  final_models <- list()
  #selected_AIC <- models %>% slice(which.min(AIC)) #not sure if i should use AIC or loglik
  #selected_loglik <- models %>% slice(which.max(loglik)) #not sure if i should use AIC or loglik
  
  for (j in 1:4){
    tryCatch({
      
      x <- paste0("fit_", model_stats$distr[j])
      
      s <- fastbw(eval(get(x)))
      terms <- row.names(s[[1]])
      print(c("terms removed: ", terms))
      for (i in 1:length(terms)){
        terms[i] <- if_else(terms[i] %in% non_linear, paste0("rcs(", terms[i], ", 3)"), terms[i] )
      }
      
      updated <- update(eval(get(x)), paste0("~ . -", paste0(terms, collapse = " -")))
      
      final_models[[j]] <- updated
      names(final_models)[j] <- paste0(model_stats$distr[j])
      
      c_index_bootstrap <- function(model, bootstrap_n) {
        set.seed (1)
        v <- rep(NA, bootstrap_n)
        for (i in 1:bootstrap_n) {
          v[i] <- (validate(model, B=1)["Dxy", "index.corrected"])
        }
        quant <- quantile(v, c(0.5, 0.025, 0.975))/2+0.5 
        return(quant)
      }
      
      c.quant <- c_index_bootstrap(updated, bootstrap_n=100)
      
      model_stats[model_stats$distr == model_stats$distr[j], ]$AIC_final <- AIC(updated)
      model_stats[model_stats$distr == model_stats$distr[j], ]$c_stat <- c.quant[1]
      model_stats[model_stats$distr == model_stats$distr[j], 6:7 ] <- c.quant[2:3]
      
      #brier
      FUN.brier <- function(year){
        outcome$pred_surv <- as.numeric(1 - predictSurvProb(updated, outcome, times=year*365.25))
        outcome$event[outcome$time>year*365.25] <- 0
        
        # Bootstrap 95% CI 
        boot_brier <- function(mydata, i){
          mydata$event <- mydata$event[i]
          mydata$pred_surv <- mydata$pred_surv[i]
          return(mean((mydata$pred_surv - mydata$event)^2, na.rm=T))
        }
        # bootstrapping with 100 replications
        set.seed(1)
        results <- boot(outcome, boot_brier, R=100)
        # get 95% confidence interval
        results.ci <- boot.ci(results, type="basic")
        brier <- c(results$t0, "lower" = results.ci$basic[4], "upper" = results.ci$basic[5])
      }
      
      brier5 <- FUN.brier(5)
      model_stats[j, 8:10] <- brier5
      
      brier10 <- FUN.brier(10)
      model_stats[j, 11:13] <- brier10
      
      
    }, error = function(e){})
    
    
  }
  models <- list(model_stats, 
                 full_models, 
                 final_models)
  names(models) <- c("stats",
                     "full_models", 
                     "final_models")
  
  
  return(models)
}

dd <- datadist(complete)
options(datadist="dd")

# no. of events
death_model <- FUN.episodic_outcomes(death_date, c("none"))
amputation_model <- FUN.episodic_outcomes(amputation, c("none")) 
mi_model <- FUN.episodic_outcomes(mi, c("none")) 

cataract_model <- FUN.episodic_outcomes(cataract, c("none")) 
stroke_model <- FUN.episodic_outcomes(stroke, c("none")) 
ulcer_skin_model <- FUN.episodic_outcomes(ulcer_skin, c("none")) 

outcomes <- do.call("list", mget(grep("_model", names(.GlobalEnv), value = TRUE)))

#chose final model - lowest AIC
FUN.final <- function(model){
  x <- model$stats %>% slice(which.min(AIC_final)) 
  model[["model"]] <- model$final_model[paste0(x$distr)]
  model <- model[-c(1:3)]
  model$distr <- paste0(x$distr)
  return(model)
}

for (i in 1:length(outcomes)){
  outcomes[[i]] <- FUN.final(eval(outcomes[[i]]))
}

save(death_model, 
     amputation_model, 
     mi_model, 
     cataract_model, 
     ulcer_skin_model, 
     stroke_model, 
     file = "full_episodic_models90.Rdata")

episodic_outcomes <- outcomes[c("death_model",
                                "amputation_model", 
                                "cataract_model",
                                "mi_model", 
                                "stroke_model", 
                                "ulcer_skin_model")]



FUN.chronic_outcomes <- function(outcome, history, rm_var){
  k <- substitute(outcome)
  h <- substitute(history)
  outcome <- subset(complete, eval(h) == F)
  
  outcome <- outcome %>% 
    
    mutate(time = case_when(is.na(eval(k)) ~ as.numeric(end_date - entry_date), 
                            TRUE ~ as.numeric( eval(k) - entry_date)),
           event = case_when(is.na(eval(k)) ~ 0,
                             TRUE ~ 1))
  
  outcome$time[outcome$time == 0] <- NA
  outcome <- outcome[!is.na(outcome$time),]
  outcome$time <- as.numeric(outcome$time)
  print(table(outcome$event))
  
  outcome$survival <- Surv(outcome$time, outcome$event)
  
  full_model <- full_model[!full_model %in% rm_var]
  
  chronic_model <- full_model[full_model != h]
  
#  fit_weibull <- psm(as.formula(paste0("survival ~ ", paste(chronic_model, collapse = " + "))),  
#                     data=outcome, model = T, x = T, y = T, dist = "weibull", control = survreg.control(maxiter = 200))
  
  fit_exp <- psm(as.formula(paste0("survival ~ ", paste(chronic_model, collapse = " + "))),
                 data=outcome, model = T, x = T, y = T, dist = "exponential", control = survreg.control(maxiter = 200))
  
  fit_loglog <- psm(as.formula(paste0("survival ~ ", paste(chronic_model, collapse = " + "))),
                    data=outcome, model = T, x = T, y = T, dist = "loglogistic", control = survreg.control(maxiter = 200))
  
  fit_lognormal <- psm(as.formula(paste0("survival ~ ", paste(chronic_model, collapse = " + "))),
                       data=outcome, model = T, x = T, y = T, dist = "lognormal", control = survreg.control(maxiter = 200))
  
  model_stats <- data.frame(distr = c(# "weibull", #comment out for renal_failure
                                      "exp", "loglog", "lognormal"), 
                            loglik_full = NA, 
                            AIC_full = NA, 
                            AIC_final = NA,
                            c_stat = NA, 
                            c_lower = NA, 
                            c_upper = NA,
                            brier5 = NA, 
                            brier5_lower = NA, 
                            brier5_upper = NA, 
                            brier10 = NA, 
                            brier10_lower = NA, 
                            brier10_upper = NA)
  
  model_stats$loglik_full <- c(# mean(fit_weibull$loglik[1], fit_weibull$loglik[2]), #comment out for renal_failure
                               mean(fit_exp$loglik[1], fit_exp$loglik[2]), 
                               mean(fit_loglog$loglik[1], fit_loglog$loglik[2]), 
                               mean(fit_lognormal$loglik[1], fit_lognormal$loglik[2]))
  
  model_stats$AIC_full <- c(# AIC(fit_weibull), #comment out for renal_failure
                            AIC(fit_exp), 
                            AIC(fit_loglog), 
                            AIC(fit_lognormal))
  
  full_models <- list(# fit_weibull, #comment out for renal_failure
                      fit_exp, 
                      fit_loglog, 
                      fit_lognormal)
  
  names(full_models) <- c(# "weibull", #comment out for renal_failure
                          "exp", "loglog", "lognormal")
  final_models <- list()
  
  for (j in 1:nrow(model_stats)){
    tryCatch({
      
      x <- paste0("fit_", model_stats$distr[j])
      
      s <- fastbw(eval(get(x)))
      terms <- row.names(s[[1]])
      print(c("terms removed: ", terms))
      for (i in 1:length(terms)){
        terms[i] <- if_else(terms[i] %in% non_linear, paste0("rcs(", terms[i], ", 3)"), terms[i] )
      }
      
      updated <- update(eval(get(x)), paste0("~ . -", paste0(terms, collapse = " -")))
      
      final_models[[j]] <- updated
      names(final_models)[j] <- paste0(model_stats$distr[j])
      
      c_index_bootstrap <- function(model, bootstrap_n) {
        set.seed (1)
        v <- rep(NA, bootstrap_n)
        for (i in 1:bootstrap_n) {
          v[i] <- (validate(model, B=1)["Dxy", "index.corrected"])
        }
        quant <- quantile(v, c(0.5, 0.025, 0.975))/2+0.5 
        return(quant)
      }
      
      c.quant <- c_index_bootstrap(updated, bootstrap_n=100)
      
      model_stats[model_stats$distr == model_stats$distr[j], ]$AIC_final <- AIC(updated)
      model_stats[model_stats$distr == model_stats$distr[j], ]$c_stat <- c.quant[1]
      model_stats[model_stats$distr == model_stats$distr[j], 6:7 ] <- c.quant[2:3]
      
      #brier
      FUN.brier <- function(year){
        outcome$pred_surv <- as.numeric(1 - predictSurvProb(updated, outcome, times=year*365.25))
        outcome$event[outcome$time>year*365.25] <- 0
        
        # Bootstrap 95% CI 
        boot_brier <- function(mydata, i){
          mydata$event <- mydata$event[i]
          mydata$pred_surv <- mydata$pred_surv[i]
          return(mean((mydata$pred_surv - mydata$event)^2, na.rm=T))
        }
        # bootstrapping with 100 replications
        set.seed(1)
        results <- boot(outcome, boot_brier, R=100)
        # get 95% confidence interval
        results.ci <- boot.ci(results, type="basic")
        brier <- c(results$t0, "lower" = results.ci$basic[4], "upper" = results.ci$basic[5])
      }
      brier5 <- FUN.brier(5)
      model_stats[j, 8:10] <- brier5
      
      brier10 <- FUN.brier(10)
      model_stats[j, 11:13] <- brier10
      
      
      
    }, error = function(e){})
    
    
  }
  models <- list(model_stats, 
                 full_models, 
                 final_models)
  names(models) <- c("stats",
                     "full_models", 
                     "final_models")
  
  return(models)
}

dd <- datadist(complete)
options(datadist="dd")

pvd_model <- FUN.chronic_outcomes(pvd, history_pvd, c("none")) 
ihd_model <- FUN.chronic_outcomes(ihd, history_ihd, c("history_mi")) 
heart_failure_model <- FUN.chronic_outcomes(heart_failure, history_heart_failure, c("none")) 
proteinuria_model <- FUN.chronic_outcomes(proteinuria, history_proteinuria, c("none")) 
neuropathy_model <- FUN.chronic_outcomes(neuropathy, history_neuropathy, c("none")) 
haemodialysis_model <- FUN.chronic_outcomes(haemodialysis, history_haemodialysis, c("none"))
retinopathy_all_model <- FUN.chronic_outcomes(retinopathy_all, history_retinopathy_all, c("none"))
renal_failure_model <- FUN.chronic_outcomes(renal_failure, history_renal_failure, c("egfr", "history_proteinuria")) 
outcomes <- do.call("list", mget(grep("_model", names(.GlobalEnv), value = TRUE)))
# outcomes <- do.call("list", mget(c("pvd_model", "ihd_model", "heart_failure_model", "proteinuria_model", "neuropathy_model", "haemodialysis_model", "retinopathy_all_model", "proteinuria_model")))


#chose final model - lowest AIC
FUN.final <- function(model){
  x <- model$stats %>% slice(which.min(AIC_final)) 
  model[["model"]] <- model$final_model[paste0(x$distr)]
  model <- model[-c(1:3)]
  model$distr <- paste0(x$distr)
  return(model)
}

for (i in 1:length(outcomes)){
  outcomes[[i]] <- FUN.final(eval(outcomes[[i]]))
}


chronic_outcomes <- outcomes
                             
