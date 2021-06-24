library(rms)
rm(list = ls())
load("outcomes_model/final_episodic_models90.Rdata") # 6 outcomes
load("outcomes_model/final_chronic_models.Rdata") # 6 outcomes
load("outcomes_model/final_dm_model90.Rdata") # 1 outcome

# total outcomes by year
all_models <- c(chronic_outcomes, episodic_outcomes, list(dm_model = dm_model))
 rm(chronic_outcomes, episodic_outcomes, dm_model); gc()
#  all patients

set.seed(123)

start_time <- Sys.time()

FUN.run_outcomes <- function(x, year) {
  x[year, "amputation"] <- FUN.outcomes(all_models[["amputation_model"]][[1]][[1]], x[year,], year)
  x[year, "cataract"] <- FUN.outcomes(all_models[["cataract_model"]][[1]][[1]], x[year,], year)
  x[year, "ihd"] <- ifelse(x[year, "history_ihd"] == 1, 1, FUN.outcomes(all_models[["ihd_model"]][[1]][[1]], x[year,], year))
  x[year, "heart_failure"] <- ifelse(x[year, "history_heart_failure"] == 1, 1, FUN.outcomes(all_models[["heart_failure_model"]][[1]][[1]], x[year,], year))
  x[year, "mi"] <- FUN.outcomes(all_models[["mi_model"]][[1]][[1]], x[year,], year)
  x[year, "neuropathy"] <- ifelse(x[year, "history_neuropathy"] == 1, 1, FUN.outcomes(all_models[["neuropathy_model"]][[1]][[1]], x[year,], year))
  #  x[year, "proteinuria"] <- ifelse(x[year, "history_proteinuria"] == 1, 1, FUN.outcomes(all_models[["proteinuria_model"]][[1]][[1]], x[year,], year))
  x[year, "renal_failure"] <- ifelse(x[year, "history_renal_failure"] == 1, 1, FUN.outcomes(all_models[["renal_failure_model"]][[1]][[1]], x[year,], year))
  x[year, "stroke"] <- FUN.outcomes(all_models[["stroke_model"]][[1]][[1]], x[year,], year)
  x[year, "ulcer_skin"] <- FUN.outcomes(all_models[["ulcer_skin_model"]][[1]][[1]], x[year,], year)
  x[year, "pvd"] <- ifelse(x[year, "history_pvd"] == 1, 1, FUN.outcomes(all_models[["pvd_model"]][[1]][[1]], x[year,], year))
  x[year, "retinopathy"] <- ifelse(x[year, "history_retinopathy"] == 1, 1, FUN.outcomes(all_models[["retinopathy_model"]][[1]][[1]], x[year,], year))
  # x[year, "haemodialysis"] <- ifelse(x[year, "history_haemodialysis"] == 1, 1, FUN.outcomes(all_models[["haemodialysis_model"]][[1]][[1]], x[year,], year))
  x[year, "death"] <- FUN.outcomes(all_models[["death_model"]][[1]][[1]], x[year,], year)
  # dm_model
  if (x[year, "status"] == 1) {
    x[year, "status"] <- FUN.outcomes(all_models[["dm_model"]][[1]][[1]], x[year,], year) + 1
  } else {x[year, "status"] <- 2
  }
  return(x)
}


patient <- lapply(patient, function(x) FUN.run_outcomes(x, 1))


FUN.outcomes2 <- function(model, patient_previous, patient_current, year){
  prob <- survest(model, patient_current, times = 365.25*(year))$surv/survest(model, patient_previous, times = 365.25*(year-1))$surv
  prob <- min(prob, 1)
  sample(c(1, 0), size = 1, replace = T, prob = c(1-prob, prob))
}

FUN.outcomes2_death <- function(model, patient_previous, patient_current, year){
  prob <- survest(model, patient_current, times = 365.25*(year-1))$surv - survest(model, patient_current, times = 365.25*(year))$surv
  prob <- max(0, prob)
  sample(c(1, 0), size = 1, replace = T, prob = c(prob, 1-prob))
}

FUN.run_outcomes2 <- function(x, year) {
  x[year, "amputation"] <- FUN.outcomes2(all_models[["amputation_model"]][[1]][[1]], x[year-1,], x[year,], year)
  x[year, "cataract"] <- FUN.outcomes2(all_models[["cataract_model"]][[1]][[1]], x[year-1,], x[year,], year)
  x[year, "ihd"] <- ifelse(x[year, "history_ihd"] == 1, 1, FUN.outcomes2(all_models[["ihd_model"]][[1]][[1]], x[year-1,], x[year,], year))
  x[year, "heart_failure"] <- ifelse(x[year, "history_heart_failure"] == 1, 1, FUN.outcomes2(all_models[["heart_failure_model"]][[1]][[1]], x[year-1,], x[year,], year))
  x[year, "mi"] <- FUN.outcomes2(all_models[["mi_model"]][[1]][[1]], x[year-1,], x[year,], year)
  x[year, "neuropathy"] <- ifelse(x[year, "history_neuropathy"] == 1, 1, FUN.outcomes2(all_models[["neuropathy_model"]][[1]][[1]], x[year-1,], x[year,], year))
  #  x[year, "proteinuria"] <- ifelse(x[year, "history_proteinuria"] == 1, 1, FUN.outcomes2(all_models[["proteinuria_model"]][[1]][[1]], x[year-1,], x[year,], year))
  x[year, "renal_failure"] <- ifelse(x[year, "history_renal_failure"] == 1, 1, FUN.outcomes2(all_models[["renal_failure_model"]][[1]][[1]], x[year-1,], x[year,], year))
  x[year, "stroke"] <- FUN.outcomes2(all_models[["stroke_model"]][[1]][[1]], x[year-1,], x[year,], year)
  x[year, "ulcer_skin"] <- FUN.outcomes2(all_models[["ulcer_skin_model"]][[1]][[1]], x[year-1,], x[year,], year)
  x[year, "pvd"] <- ifelse(x[year, "history_pvd"] == 1, 1, FUN.outcomes2(all_models[["pvd_model"]][[1]][[1]], x[year-1,], x[year,], year))
  x[year, "retinopathy"] <- ifelse(x[year, "history_retinopathy"] == 1, 1, FUN.outcomes2(all_models[["retinopathy_model"]][[1]][[1]], x[year-1,], x[year,], year))
  # x[year, "haemodialysis"] <- ifelse(x[year, "history_haemodialysis"] == 1, 1, FUN.outcomes2(all_models[["haemodialysis_model"]][[1]][[1]], x[year-1,], x[year,], year))
  x[year, "death"] <- FUN.outcomes2_death(all_models[["death_model"]][[1]][[1]], x[year-1,], x[year,], year)
  # dm_model
  if (x[year, "status"] == 1) {
    x[year, "status"] <- FUN.outcomes2(all_models[["dm_model"]][[1]][[1]], x[year-1,], x[year,], year) + 1
  } else {x[year, "status"] <- 2
  }
  return(x)
}

FUN.run_additional_year <- function(x, year){
  if (x[year-1, "death"] == 0 & !is.na(x[year-1, "death"])) {
    x[year, "history_amputation"] <- max(x[year-1, "history_amputation"], x[year-1, "amputation"])
    x[year, "history_cataract"] <- max(x[year-1, "history_cataract"], x[year-1, "cataract"])
    x[year, "history_ihd"] <- max(x[year-1, "history_ihd"], x[year-1, "ihd"])
    x[year, "history_heart_failure"] <- max(x[year-1, "history_heart_failure"], x[year-1, "heart_failure"])
    x[year, "history_mi"] <- max(x[year-1, "history_mi"], x[year-1, "mi"])
    x[year, "history_neuropathy"] <- max(x[year-1, "history_neuropathy"], x[year-1, "neuropathy"])
    #    x[year, "history_proteinuria"] <- max(x[year-1, "history_proteinuria"], x[year-1, "proteinuria"])
    x[year, "history_stroke"] <- max(x[year-1, "history_stroke"], x[year-1, "stroke"])
    x[year, "history_ulcer_skin"] <- max(x[year-1, "history_ulcer_skin"], x[year-1, "ulcer_skin"])
    x[year, "history_pvd"] <- max(x[year-1, "history_pvd"], x[year-1, "pvd"])
    x[year, "history_retinopathy"] <- max(x[year-1, "history_retinopathy"], x[year-1, "retinopathy"])
    x[year, "history_renal_failure"] <- max(x[year-1, "history_renal_failure"], x[year-1, "renal_failure"])
    # x[year, "history_haemodialysis"] <- max(x[year-1, "history_haemodialysis"], x[year-1, "haemodialysis"])
    x[year, "status"] <- x[year-1, "status"]
    FUN.run_outcomes2(x, year)
  }
  else {
    x[year, "death"] <- NA
    return(x)
  }
}

patient <- lapply(patient, function(x) FUN.run_additional_year(x, 2))
patient <- lapply(patient, function(x) FUN.run_additional_year(x, 3))
patient <- lapply(patient, function(x) FUN.run_additional_year(x, 4))
patient <- lapply(patient, function(x) FUN.run_additional_year(x, 5))
patient <- lapply(patient, function(x) FUN.run_additional_year(x, 6))
patient <- lapply(patient, function(x) FUN.run_additional_year(x, 7))
patient <- lapply(patient, function(x) FUN.run_additional_year(x, 8))
patient <- lapply(patient, function(x) FUN.run_additional_year(x, 9))
patient <- lapply(patient, function(x) FUN.run_additional_year(x, 10))
patient <- lapply(patient, function(x) FUN.run_additional_year(x, 11))
patient <- lapply(patient, function(x) FUN.run_additional_year(x, 12))
patient <- lapply(patient, function(x) FUN.run_additional_year(x, 13))
patient <- lapply(patient, function(x) FUN.run_additional_year(x, 14))
patient <- lapply(patient, function(x) FUN.run_additional_year(x, 15))
patient <- lapply(patient, function(x) FUN.run_additional_year(x, 16))
patient <- lapply(patient, function(x) FUN.run_additional_year(x, 17))
patient <- lapply(patient, function(x) FUN.run_additional_year(x, 18))
patient <- lapply(patient, function(x) FUN.run_additional_year(x, 19))
patient <- lapply(patient, function(x) FUN.run_additional_year(x, 20))

