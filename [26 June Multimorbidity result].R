library("survival")
library("survminer")
library("foreign")
library("tidyverse")
library("haven")
library("dplyr")

data<- read_sav ("/Users/evonwong/Library/CloudStorage/OneDrive-TheEducationUniversityofHongKong/Evon/UKBB/Clustering/[Multimorbidity filtered data].sav")
head(data)
# Summary of the dataset
summary(data)
# View the structure of the dataset
str(data)



################MM ref1##########

selected_data_MMref1 <- data %>%
  select(MM_onset_end_death_time, MM_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_MMref1_model <- coxph(Surv(MM_onset_end_death_time, MM_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_MMref1) 
summary (cox_MMref1_model)



# Example of running separate models for each income group
income_levels <- levels(selected_data_MMref1$Ave_Household_Income_mir_v2)
models_by_income <- lapply(income_levels, function(income) {
  data_subset <- filter(selected_data_MMref1, Ave_Household_Income_mir_v2 == income)
  coxph(Surv(MM_onset_end_death_time, MM_onset_end_death_0_1) ~ 
          Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Cluster_k4_v2_recode, 
        data = data_subset)
})

# Example of summarizing one of the models
summary(models_by_income[[1]])  # Replace 1 with appropriate index for specific income level
# Example of summarizing one of the models
summary(models_by_income[[2]])  # Replace 1 with appropriate index for specific income level



################MM ref2##########

selected_data_MMref2 <- data %>%
  select(MM_onset_end_death_time, MM_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_MMref2_model <- coxph(Surv(MM_onset_end_death_time, MM_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_MMref2) 
summary (cox_MMref2_model)




################MM ref3##########

selected_data_MMref3 <- data %>%
  select(MM_onset_end_death_time, MM_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_MMref3_model <- coxph(Surv(MM_onset_end_death_time, MM_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_MMref3) 
summary (cox_MMref3_model)



################MM ref4##########

selected_data_MMref4 <- data %>%
  select(MM_onset_end_death_time, MM_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_MMref4_model <- coxph(Surv(MM_onset_end_death_time, MM_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_MMref4) 
summary (cox_MMref4_model)



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
fitted_models_MM <- list()

# Loop to fit models and store results
for (i in names(models)) {
  fitted_models_MM [[i]] <- survreg(Surv(MM_onset_end_death_time, MM_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_MMref1, dist = models[[i]])
}

# View summaries and calculate AIC/BIC
aic_values_MM <- sapply(fitted_models_MM , AIC)
bic_values_MM <- sapply(fitted_models_MM , BIC)

# Print summaries
lapply(fitted_models_MM , summary)

# Print AIC and BIC values
print(aic_values_MM)
print(bic_values_MM)


# Calculate and display time ratios
time_ratios_MM <- exp(coef(fitted_models_MM [["Loglogistic"]]))
print(time_ratios_MM)

# Calculate and display time ratios for the Loglogistic model
# Extract coefficients and their standard errors
coef_Loglogistic_MM <- coef(fitted_models_MM[["Loglogistic"]])
se_Loglogistic_MM <- sqrt(diag(vcov(fitted_models_MM[["Loglogistic"]])))

# Calculate 95% confidence intervals on the log-time scale
ci_low_Loglogistic_MM <- coef_Loglogistic_MM - 1.96 * se_Loglogistic_MM
ci_high_Loglogistic_MM <- coef_Loglogistic_MM + 1.96 * se_Loglogistic_MM

# Transform confidence intervals to the time scale
time_ratio_lower_Loglogistic_MM <- exp(ci_low_Loglogistic_MM)
time_ratio_upper_Loglogistic_MM <- exp(ci_high_Loglogistic_MM)

# Calculate time ratios
time_ratios_Loglogistic_MM <- exp(coef_Loglogistic_MM)

# Print results for Loglogistic model
print("Time Ratios for Loglogistic Model:")
print(time_ratios_Loglogistic_MM)
print("95% Confidence Intervals Lower Bounds:")
print(time_ratio_lower_Loglogistic_MM)
print("95% Confidence Intervals Upper Bounds:")
print(time_ratio_upper_Loglogistic_MM)
