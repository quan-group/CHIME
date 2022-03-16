
library(rms)
library(survival)

load("CHIME_models.Rdata")


# example 
# read.csv("sample_data.csv) or readxl

patient = data.frame(status = 2,  #diabetes
                     age = 63,    
                     female = 1,     
                     duration = 5, 
                     smoking = "Ex",
                     #biomarkers
                     hba1c = 7.7,
                     sbp = 131,
                     dbp = 79,
                     #     tri = tri_t1,
                     hdl = 1.27,
                     #    ldl = ldl_t1,
                     bmi = 26,
                     egfr = 70,
                     wbc = 9.5, 
                     haemoglobin = 12.5,
                     # meds
                     #          insulin = 0,
                     #          non_insulin = 1,
                     #         bprx = 1,
                     #        statin = 1,
                     #history
                     history_af = 0,
                     history_amputation = 0,
                     history_cataract = 0,
                     history_ihd =  1,
                     history_heart_failure = 1,
                     history_mi = 1, 
                     history_neuropathy = 0,
                     #  history_proteinuria = rbinom(1, 1, history_proteinuria),
                     history_stroke = 0,
                     history_ulcer_skin = 0,
                     history_pvd = 0,
                     history_retinopathy = 0,
                     history_haemodialysis = 0, 
                     history_renal_failure = 0)



patient$smoking <- as.factor(patient$smoking)
patient$egfr <- cut(patient$egfr, 
                    breaks = c(Inf, 90, 60, 45, 30, 15, -Inf), 
                    labels = c(1, 2, 3, 4, 5, 6)) #labeled decreasing to increasing numbers, order doesn't matter
patient$egfr <- factor(patient$egfr, 
                       levels = c(6, 5, 4, 3, 2, 1), 
                       labels = c("normal", "mild", "mild-moderate", "moderate-severe", "severe", "kidney-failure"))

# set to 5 years
yr <- 5
# risk of event
1 - survest(mi_model, patient, times = 365.25*(yr))$surv
# 11.9% risk
