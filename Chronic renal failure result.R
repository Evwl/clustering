install.packages(c("survival", "survminer"))
library("survival")
library("survminer")
library("foreign")
library("tidyverse")
library("haven")
library("dplyr")

data<- read_sav ("/Users/evonwong/Library/CloudStorage/OneDrive-TheEducationUniversityofHongKong/Evon/UKBB/Clustering/[26 June crf].sav")
# View the first few rows of the dataset
head(data)
# Summary of the dataset
summary(data)
# View the structure of the dataset
str(data)

############ref1
selected_data_crf <- data %>%
  select(N18_C_renal_failure_onset_end_death_time, N18_C_renal_failure_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_crf_model <- coxph(Surv(N18_C_renal_failure_onset_end_death_time, N18_C_renal_failure_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_crf) 
summary (cox_crf_model)

# Example of running separate models for each income group
income_levels <- levels(selected_data_crf$Ave_Household_Income_mir_v2)
models_by_income <- lapply(income_levels, function(income) {
  data_subset <- filter(selected_data_crf, Ave_Household_Income_mir_v2 == income)
  coxph(Surv(N18_C_renal_failure_onset_end_death_time, N18_C_renal_failure_onset_end_death_0_1) ~ 
          Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Cluster_k4_v2_recode, 
        data = data_subset)
})

# Example of summarizing one of the models
summary(models_by_income[[1]])  # Replace 1 with appropriate index for specific income level
# Example of summarizing one of the models
summary(models_by_income[[2]])  # Replace 1 with appropriate index for specific income level



############crf ref2
selected_data_crf_ref2 <- data %>%
  select(N18_C_renal_failure_onset_end_death_time, N18_C_renal_failure_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_crf_ref2_model <- coxph(Surv(N18_C_renal_failure_onset_end_death_time, N18_C_renal_failure_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_crf_ref2) 
summary (cox_crf_ref2_model)

############crf ref3
selected_data_crf_ref3 <- data %>%
  select(N18_C_renal_failure_onset_end_death_time, N18_C_renal_failure_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_crf_ref3_model <- coxph(Surv(N18_C_renal_failure_onset_end_death_time, N18_C_renal_failure_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_crf_ref3) 
summary (cox_crf_ref3_model)

############crf ref4
selected_data_crf_ref4 <- data %>%
  select(N18_C_renal_failure_onset_end_death_time, N18_C_renal_failure_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_crf_ref4_model <- coxph(Surv(N18_C_renal_failure_onset_end_death_time, N18_C_renal_failure_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_crf_ref4) 
summary (cox_crf_ref4_model)

################# AFT models ############################################

# Fit AFT models with different distributions
models <- list(
  Weibull = "weibull",
  Exponential = "exponential",
  Gaussian = "gaussian",
  Logistic = "logistic",
  Lognormal = "lognormal",
  Loglogistic = "loglogistic"
)
# Initialize list to store models
fitted_models_crf <- list()

# Loop to fit models and store results
for (i in names(models)) {
  fitted_models_crf [[i]] <- survreg(Surv(N18_C_renal_failure_onset_end_death_time, N18_C_renal_failure_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_crf, dist = models[[i]])
}

# View summaries and calculate AIC/BIC
aic_values_crf <- sapply(fitted_models_crf , AIC)
bic_values_crf <- sapply(fitted_models_crf , BIC)

# Print summaries
lapply(fitted_models_crf , summary)

# Print AIC and BIC values
print(aic_values_crf)
print(bic_values_crf)


# Calculate and display time ratios
time_ratios_crf <- exp(coef(fitted_models_crf [["Weibull"]]))
print(time_ratios_crf)



# Calculate and display time ratios for Weibull model
# Extract coefficients and their standard errors
coef_weibull <- coef(fitted_models_crf[["Weibull"]])
se_weibull <- sqrt(diag(vcov(fitted_models_crf[["Weibull"]])))

# Calculate 95% confidence intervals on the log-time scale
ci_low_weibull <- coef_weibull - 1.96 * se_weibull
ci_high_weibull <- coef_weibull + 1.96 * se_weibull

# Transform confidence intervals to the time scale
time_ratio_lower_weibull <- exp(ci_low_weibull)
time_ratio_upper_weibull <- exp(ci_high_weibull)

# Calculate time ratios
time_ratios_weibull <- exp(coef_weibull)

# Print results for Weibull model
print("Time Ratios for Weibull Model:")
print(time_ratios_weibull)
print("95% Confidence Intervals Lower Bounds:")
print(time_ratio_lower_weibull)
print("95% Confidence Intervals Upper Bounds:")
print(time_ratio_upper_weibull)
