install.packages(c("survival", "survminer"))
library("survival")
library("survminer")
library("foreign")
library("tidyverse")
library("haven")
library("dplyr")

data<- read_sav ("/Users/evonwong/Library/CloudStorage/OneDrive-TheEducationUniversityofHongKong/Evon/UKBB/Clustering/[26 June Bipolar].sav")

# View the first few rows of the dataset
head(data)
# Summary of the dataset
summary(data)
# View the structure of the dataset
str(data)
####################################Bipolar############
######ref1#####
selected_data_bipolar <- data %>%
  select(Bipolar_dis_F31_onset_end_death_time, Bipolar_dis_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode)
  )
library(survival)
cox_bipolar_model <- coxph(Surv(Bipolar_dis_F31_onset_end_death_time, Bipolar_dis_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_bipolar) 
summary (cox_bipolar_model)


# Example of running separate models for each income group
income_levels <- levels(selected_data_bipolar$Ave_Household_Income_mir_v2)
models_by_income <- lapply(income_levels, function(income) {
  data_subset <- filter(selected_data_bipolar, Ave_Household_Income_mir_v2 == income)
  coxph(Surv(Bipolar_dis_F31_onset_end_death_time, Bipolar_dis_onset_end_death_0_1) ~ 
          Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Cluster_k4_v2_recode, 
        data = data_subset)
})

# Example of summarizing one of the models
summary(models_by_income[[1]])  # Replace 1 with appropriate index for specific income level
# Example of summarizing one of the models
summary(models_by_income[[2]])  # Replace 1 with appropriate index for specific income level


######ref2#####
selected_data_bipolar_ref2 <- data %>%
  select(Bipolar_dis_F31_onset_end_death_time, Bipolar_dis_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_bipolar_ref2_model <- coxph(Surv(Bipolar_dis_F31_onset_end_death_time, Bipolar_dis_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_bipolar_ref2) 
summary (cox_bipolar_ref2_model)

######ref3#####
selected_data_bipolar_ref3 <- data %>%
  select(Bipolar_dis_F31_onset_end_death_time, Bipolar_dis_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_bipolar_ref3_model <- coxph(Surv(Bipolar_dis_F31_onset_end_death_time, Bipolar_dis_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_bipolar_ref3) 
summary (cox_bipolar_ref3_model)

######ref4#####
selected_data_bipolar_ref4 <- data %>%
  select(Bipolar_dis_F31_onset_end_death_time, Bipolar_dis_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_bipolar_ref4_model <- coxph(Surv(Bipolar_dis_F31_onset_end_death_time, Bipolar_dis_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_bipolar_ref4) 
summary (cox_bipolar_ref4_model)






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
fitted_models_bipolar <- list()

# Loop to fit models and store results
for (i in names(models)) {
  fitted_models_bipolar [[i]] <- survreg(Surv(Bipolar_dis_F31_onset_end_death_time, Bipolar_dis_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_bipolar, dist = models[[i]])
}

# View summaries and calculate AIC/BIC
aic_values_bipolar <- sapply(fitted_models_bipolar , AIC)
bic_values_bipolar <- sapply(fitted_models_bipolar , BIC)

# Print summaries
lapply(fitted_models_bipolar , summary)

# Print AIC and BIC values
print(aic_values_bipolar)
print(bic_values_bipolar)


# Calculate and display time ratios
time_ratios_bipolar <- exp(coef(fitted_models_bipolar [["Lognormal"]]))
print(time_ratios_bipolar)

# Calculate and display time ratios for the Lognormal model
# Extract coefficients and their standard errors
coef_Lognormal_bipolar <- coef(fitted_models_bipolar[["Lognormal"]])
se_Lognormal_bipolar <- sqrt(diag(vcov(fitted_models_bipolar[["Lognormal"]])))

# Calculate 95% confidence intervals on the log-time scale
ci_low_Lognormal_bipolar <- coef_Lognormal_bipolar - 1.96 * se_Lognormal_bipolar
ci_high_Lognormal_bipolar <- coef_Lognormal_bipolar + 1.96 * se_Lognormal_bipolar

# Transform confidence intervals to the time scale
time_ratio_lower_Lognormal_bipolar <- exp(ci_low_Lognormal_bipolar)
time_ratio_upper_Lognormal_bipolar <- exp(ci_high_Lognormal_bipolar)

# Calculate time ratios
time_ratios_Lognormal_bipolar <- exp(coef_Lognormal_bipolar)

# Print results for Lognormal model
print("Time Ratios for Lognormal Model:")
print(time_ratios_Lognormal_bipolar)
print("95% Confidence Intervals Lower Bounds:")
print(time_ratio_lower_Lognormal_bipolar)
print("95% Confidence Intervals Upper Bounds:")
print(time_ratio_upper_Lognormal_bipolar)
