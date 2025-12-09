install.packages(c("survival", "survminer"))
library("survival")
library("survminer")
library("foreign")
library("tidyverse")
library("haven")
library("dplyr")

data<- read_sav ("/Users/evonwong/Library/CloudStorage/OneDrive-TheEducationUniversityofHongKong/Evon/UKBB/Clustering/[26 June DM].sav")
# View the first few rows of the dataset
head(data)
# Summary of the dataset
summary(data)
# View the structure of the dataset
str(data)

################Diabetes DMtype 2 ref1##########
selected_data_DM <- data %>%
  select(DM_T2_onset_end_death_time, DM_T2_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode)
  )
library(survival)
cox_DM_model <- coxph(Surv(DM_T2_onset_end_death_time, DM_T2_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_DM) 
summary (cox_DM_model)

# Example of running separate models for each income group
income_levels <- levels(selected_data_DM$Ave_Household_Income_mir_v2)
models_by_income <- lapply(income_levels, function(income) {
  data_subset <- filter(selected_data_DM, Ave_Household_Income_mir_v2 == income)
  coxph(Surv(DM_T2_onset_end_death_time, DM_T2_onset_end_death_0_1) ~ 
          Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Cluster_k4_v2_recode, 
        data = data_subset)
})

# Example of summarizing one of the models
summary(models_by_income[[1]])  # Replace 1 with appropriate index for specific income level
# Example of summarizing one of the models
summary(models_by_income[[2]])  # Replace 1 with appropriate index for specific income level


################Diabetes DMtype 2 ref2##########
selected_data_DMref2 <- data %>%
  select(DM_T2_onset_end_death_time, DM_T2_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_DMref2_model <- coxph(Surv(DM_T2_onset_end_death_time, DM_T2_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_DMref2) 
summary (cox_DMref2_model)

################Diabetes DMtype 2 ref3##########
selected_data_DMref3 <- data %>%
  select(DM_T2_onset_end_death_time, DM_T2_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_DMref3_model <- coxph(Surv(DM_T2_onset_end_death_time, DM_T2_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_DMref3) 
summary (cox_DMref3_model)

################Diabetes DMtype 2 ref4 ##########
selected_data_DMref4 <- data %>%
  select(DM_T2_onset_end_death_time, DM_T2_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_DMref4_model <- coxph(Surv(DM_T2_onset_end_death_time, DM_T2_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_DMref4) 
summary (cox_DMref4_model)


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
fitted_models_DM <- list()

# Loop to fit models and store results
for (i in names(models)) {
  fitted_models_DM [[i]] <- survreg(Surv(DM_T2_onset_end_death_time, DM_T2_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_DM, dist = models[[i]])
}

# View summaries and calculate AIC/BIC
aic_values_DM <- sapply(fitted_models_DM , AIC)
bic_values_DM <- sapply(fitted_models_DM , BIC)

# Print summaries
lapply(fitted_models_DM , summary)

# Print AIC and BIC values
print(aic_values_DM)
print(bic_values_DM)


# Calculate and display time ratios
time_ratios_DM <- exp(coef(fitted_models_DM [["Loglogistic"]]))
print(time_ratios_DM)


# Calculate and display time ratios for the Loglogistic model
# Extract coefficients and their standard errors
coef_loglogistic_DM <- coef(fitted_models_DM[["Loglogistic"]])
se_loglogistic_DM <- sqrt(diag(vcov(fitted_models_DM[["Loglogistic"]])))

# Calculate 95% confidence intervals on the log-time scale
ci_low_loglogistic_DM <- coef_loglogistic_DM - 1.96 * se_loglogistic_DM
ci_high_loglogistic_DM <- coef_loglogistic_DM + 1.96 * se_loglogistic_DM

# Transform confidence intervals to the time scale
time_ratio_lower_loglogistic_DM <- exp(ci_low_loglogistic_DM)
time_ratio_upper_loglogistic_DM <- exp(ci_high_loglogistic_DM)

# Calculate time ratios
time_ratios_loglogistic_DM <- exp(coef_loglogistic_DM)

# Print results for Loglogistic model
print("Time Ratios for Loglogistic Model:")
print(time_ratios_loglogistic_DM)
print("95% Confidence Intervals Lower Bounds:")
print(time_ratio_lower_loglogistic_DM)
print("95% Confidence Intervals Upper Bounds:")
print(time_ratio_upper_loglogistic_DM)
