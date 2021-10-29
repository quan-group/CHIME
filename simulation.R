
# load models -------------------------------------------------------------
library(rms)
library(dplyr)
library(data.table)

load("CHIME_models.Rdata")


# sample data -------------------------------------------------------------

patient <- readRDS("sample_data.rds")

patient <- patient %>%
  slice(rep(1:n(), each = 20)) 
patient$year <- rep_len(c(1:20), nrow(patient))
patient$age <- patient$age + patient$year-1 # baseline = 0
patient$duration <- patient$duration + patient$year-1 # baseline = 0


# dm: patient$status = 2
# category levels
# levels(patient$smoking)
# patient$egfr <- cut(p$egfr, breaks = c(Inf, 90, 60, 45, 30, 15, -Inf), labels = c(1, 2, 3, 4, 5, 6)) 
# patient$egfr <- factor(p$egfr, levels = c(6, 5, 4, 3, 2, 1), labels = c("normal", "mild", "mild-moderate", "moderate-severe", "severe", "kidney-failure"))
# levels(patient$egfr)


# simulation ----------------------------------------------------------
set.seed(123)

FUN.outcomes <- function(model, patient, yr){
  prob <- survest(model, patient, times = 365.25*(yr))$surv
  sapply(prob, function(x) sample(c(1, 0), size = 1, replace = T, prob = c(1-x, x)))
}

FUN.run_outcomes <- function(x, yr) {
  x[x$year==yr, "amputation"] <- FUN.outcomes(amputation_model, x[x$year==yr,], yr)
  x[x$year==yr, "cataract"] <- FUN.outcomes(cataract_model, x[x$year==yr,], yr)
  x[x$year==yr, "ihd"] <- ifelse(x[x$year==yr, "history_ihd"] == 1, 1, FUN.outcomes(ihd_model, x[x$year==yr,], yr))
  x[x$year==yr, "heart_failure"] <- ifelse(x[x$year==yr, "history_heart_failure"] == 1, 1, FUN.outcomes(heart_failure_model, x[x$year==yr,], yr))
  x[x$year==yr, "mi"] <- FUN.outcomes(mi_model, x[x$year==yr,], yr)
  x[x$year==yr, "neuropathy"] <- ifelse(x[x$year==yr, "history_neuropathy"] == 1, 1, FUN.outcomes(neuropathy_model, x[x$year==yr,], yr))
  #  x[x$year==yr, "proteinuria"] <- ifelse(x[x$year==yr, "history_proteinuria"] == 1, 1, FUN.outcomes(proteinuria_model, x[x$year==yr,], yr))
  x[x$year==yr, "renal_failure"] <- ifelse(x[x$year==yr, "history_renal_failure"] == 1, 1, FUN.outcomes(renal_failure_model, x[x$year==yr,], yr))
  x[x$year==yr, "stroke"] <- FUN.outcomes(stroke_model, x[x$year==yr,], yr)
  x[x$year==yr, "ulcer_skin"] <- FUN.outcomes(ulcer_skin_model, x[x$year==yr,], yr)
  x[x$year==yr, "pvd"] <- ifelse(x[x$year==yr, "history_pvd"] == 1, 1, FUN.outcomes(pvd_model, x[x$year==yr,], yr))
  x[x$year==yr, "retinopathy"] <- ifelse(x[x$year==yr, "history_retinopathy"] == 1, 1, FUN.outcomes(retinopathy_model, x[x$year==yr,], yr))
  # x[x$year==yr, "haemodialysis"] <- ifelse(x[year, "history_haemodialysis"] == 1, 1, FUN.outcomes(haemodialysis_model, x[x$year==yr,], yr))
  x[x$year==yr, "death"] <- FUN.outcomes(death_model, x[x$year==yr,], yr)
  return(x)
}

# run dm_model if pre-dm (staus = 1)
# x[x$year==yr & x$status == 1, "status"] <- FUN.outcomes(dm_model, x[x$year==yr & x$status == 1,], yr) + 1

FUN.outcomes2 <- function(model, patient_previous, patient_current, yr){
  prob <- survest(model, patient_current, times = 365.25*(yr))$surv/survest(model, patient_previous, times = 365.25*(yr-1))$surv
  prob <- pmin(prob, 1)
  sapply(prob, function(x) sample(c(1, 0), size = 1, replace = T, prob = c(1-x, x)))
}

FUN.outcomes2_death <- function(model, patient_previous, patient_current, yr){
  prob <- survest(model, patient_current, times = 365.25*(yr-1))$surv - survest(model, patient_current, times = 365.25*(yr))$surv
  prob <- pmax(0, prob)
  sapply(prob, function(x) sample(c(1, 0), size = 1, replace = T, prob = c(x, 1-x)))
}

FUN.run_outcomes2 <- function(x, yr) {
  x[x$year==yr, "amputation"] <- FUN.outcomes2(amputation_model, x[x$year==(yr-1),], x[x$year==yr,], yr)
  x[x$year==yr, "cataract"] <- FUN.outcomes2(cataract_model, x[x$year==(yr-1),], x[x$year==yr,], yr)
  x[x$year==yr, "ihd"] <- ifelse(x[x$year==yr, "history_ihd"] == 1, 1, FUN.outcomes2(ihd_model, x[x$year==(yr-1),], x[x$year==yr,], yr))
  x[x$year==yr, "heart_failure"] <- ifelse(x[x$year==yr, "history_heart_failure"] == 1, 1, FUN.outcomes2(heart_failure_model, x[x$year==(yr-1),], x[x$year==yr,], yr))
  x[x$year==yr, "mi"] <- FUN.outcomes2(mi_model, x[x$year==(yr-1),], x[x$year==yr,], yr)
  x[x$year==yr, "neuropathy"] <- ifelse(x[x$year==yr, "history_neuropathy"] == 1, 1, FUN.outcomes2(neuropathy_model, x[x$year==(yr-1),], x[x$year==yr,], yr))
  #  x[x$year==yr, "proteinuria"] <- ifelse(x[x$year==yr, "history_proteinuria"] == 1, 1, FUN.outcomes2(proteinuria_model, x[x$year==(yr-1),], x[x$year==yr,], yr))
  x[x$year==yr, "renal_failure"] <- ifelse(x[x$year==yr, "history_renal_failure"] == 1, 1, FUN.outcomes2(renal_failure_model, x[x$year==(yr-1),], x[x$year==yr,], yr))
  x[x$year==yr, "stroke"] <- FUN.outcomes2(stroke_model, x[x$year==(yr-1),], x[x$year==yr,], yr)
  x[x$year==yr, "ulcer_skin"] <- FUN.outcomes2(ulcer_skin_model, x[x$year==(yr-1),], x[x$year==yr,], yr)
  x[x$year==yr, "pvd"] <- ifelse(x[x$year==yr, "history_pvd"] == 1, 1, FUN.outcomes2(pvd_model, x[x$year==(yr-1),], x[x$year==yr,], yr))
  x[x$year==yr, "retinopathy"] <- ifelse(x[x$year==yr, "history_retinopathy"] == 1, 1, FUN.outcomes2(retinopathy_model, x[x$year==(yr-1),], x[x$year==yr,], yr))
  # x[x$year==yr, "haemodialysis"] <- ifelse(x[x$year==yr, "history_haemodialysis"] == 1, 1, FUN.outcomes2(haemodialysis_model, x[x$year==(yr-1),], x[x$year==yr,], yr))
  x[x$year==yr, "death"] <- FUN.outcomes2_death(death_model, x[x$year==(yr-1),], x[x$year==yr,], yr)
  # dm_model
  # x[x$year==yr & x$status == 1, "status"] <- FUN.outcomes(dm_model, x[x$year==yr & x$status == 1,], yr) + 1
  return(x)
}

FUN.run_additional_year <- function(x, yr){
  died <- x$serial_no[!is.na(x$death) & x$death==1]
  
  nm1 <- grep("amputation|cataract|ihd|heart_failure|^mi|^history|neuropathy|stroke|ulcer_skin|pvd|retinopathy|renal_failure|status", colnames(x), value=TRUE)
  nm2 <- paste("lag", nm1, sep="_")
  x <- data.table(x)
  x <- x[, (nm2):=lapply(.SD, function(x) c(NA, x[-.N])), by=serial_no, .SDcols=nm1]
  
  x[!(x$serial_no %in% died) & x$year==yr,] <- x[!(x$serial_no %in% died) & x$year==yr,] %>% 
    mutate(history_amputation = pmax(lag_history_amputation, lag_amputation),
           history_cataract = pmax(lag_history_cataract, lag_cataract),
           history_ihd = pmax(lag_history_ihd, lag_ihd),
           history_heart_failure = pmax(lag_history_heart_failure, lag_heart_failure),
           history_mi = pmax(lag_history_mi, lag_mi),
           history_neuropathy = pmax(lag_history_neuropathy, lag_neuropathy),
           history_stroke = pmax(lag_history_stroke, lag_stroke),
           history_ulcer_skin = pmax(lag_history_ulcer_skin, lag_ulcer_skin),
           history_pvd = pmax(lag_history_pvd, lag_pvd),
           history_retinopathy = pmax(lag_history_retinopathy, lag_retinopathy),
           history_renal_failure = pmax(lag_history_renal_failure, lag_renal_failure),
           status = pmax(lag_status))
  x <- x[, !(grepl("^lag", colnames(x))), with = F] # drop lags
  x[!(x$serial_no %in% died),] <- FUN.run_outcomes2(x[!(x$serial_no %in% died),], yr)
  return(x)
}


# simulation, years ----------------------------------------------------------------

patient <- FUN.run_outcomes(patient, 1)
patient <- FUN.run_additional_year(patient, 2)
patient <- FUN.run_additional_year(patient, 3)
patient <- FUN.run_additional_year(patient, 4)
patient <- FUN.run_additional_year(patient, 5)
patient <- FUN.run_additional_year(patient, 6)
patient <- FUN.run_additional_year(patient, 7)
patient <- FUN.run_additional_year(patient, 8)
patient <- FUN.run_additional_year(patient, 9)
patient <- FUN.run_additional_year(patient, 10)
patient <- FUN.run_additional_year(patient, 11)
patient <- FUN.run_additional_year(patient, 12)
patient <- FUN.run_additional_year(patient, 13)
patient <- FUN.run_additional_year(patient, 14)
patient <- FUN.run_additional_year(patient, 15)
patient <- FUN.run_additional_year(patient, 16)
patient <- FUN.run_additional_year(patient, 17)
patient <- FUN.run_additional_year(patient, 18)
patient <- FUN.run_additional_year(patient, 19)
patient <- FUN.run_additional_year(patient, 20)


