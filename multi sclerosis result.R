install.packages(c("survival", "survminer"))
library("survival")
library("survminer")
library("foreign")
library("tidyverse")
library("haven")
library("dplyr")

data<- read_sav ("/Users/evonwong/Library/CloudStorage/OneDrive-TheEducationUniversityofHongKong/Evon/UKBB/Clustering/[26 June MS].sav")
# View the first few rows of the dataset
head(data)
# Summary of the dataset
summary(data)
# View the structure of the dataset
str(data)

############################ref1#########
selected_data_MS_ref1 <- data %>%
  select(G35_Multi_sclerosis_onset_end_death_time, G35_Multi_sclerosis_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_MS_ref1_model <- coxph(Surv(G35_Multi_sclerosis_onset_end_death_time, G35_Multi_sclerosis_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_MS_ref1) 
summary (cox_MS_ref1_model)

# Example of running separate models for each income group
income_levels <- levels(selected_data_MS_ref1$Ave_Household_Income_mir_v2)
models_by_income <- lapply(income_levels, function(income) {
  data_subset <- filter(selected_data_MS_ref1, Ave_Household_Income_mir_v2 == income)
  coxph(Surv(G35_Multi_sclerosis_onset_end_death_time, G35_Multi_sclerosis_onset_end_death_0_1) ~ 
          Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Cluster_k4_v2_recode, 
        data = data_subset)
})

# Example of summarizing one of the models
summary(models_by_income[[1]])  # Replace 1 with appropriate index for specific income level
# Example of summarizing one of the models
summary(models_by_income[[2]])  # Replace 1 with appropriate index for specific income level






#########################ref2#######################
selected_data_MS_ref2 <- data %>%
  select(G35_Multi_sclerosis_onset_end_death_time, G35_Multi_sclerosis_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_MS_ref2_model <- coxph(Surv(G35_Multi_sclerosis_onset_end_death_time, G35_Multi_sclerosis_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_MS_ref2) 
summary (cox_MS_ref2_model)

#########################ref3#######################
selected_data_MS_ref3 <- data %>%
  select(G35_Multi_sclerosis_onset_end_death_time, G35_Multi_sclerosis_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_MS_ref3_model <- coxph(Surv(G35_Multi_sclerosis_onset_end_death_time, G35_Multi_sclerosis_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_MS_ref3) 
summary (cox_MS_ref3_model)

#########################ref4#######################
selected_data_MS_ref4 <- data %>%
  select(G35_Multi_sclerosis_onset_end_death_time, G35_Multi_sclerosis_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_MS_ref4_model <- coxph(Surv(G35_Multi_sclerosis_onset_end_death_time, G35_Multi_sclerosis_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_MS_ref4) 
summary (cox_MS_ref4_model)

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
fitted_models_MS <- list()

# Loop to fit models and store results
for (i in names(models)) {
  fitted_models_MS [[i]] <- survreg(Surv(G35_Multi_sclerosis_onset_end_death_time, G35_Multi_sclerosis_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_MS_ref1, dist = models[[i]])
}

# View summaries and calculate AIC/BIC
aic_values_MS <- sapply(fitted_models_MS , AIC)
bic_values_MS <- sapply(fitted_models_MS , BIC)

# Print summaries
lapply(fitted_models_MS , summary)

# Print AIC and BIC values
print(aic_values_MS)
print(bic_values_MS)


# Calculate and display time ratios
time_ratios_MS <- exp(coef(fitted_models_MS [["Lognormal"]]))
print(time_ratios_MS)


# Calculate and display time ratios for the Lognormal model
# Extract coefficients and their standard errors
coef_lognormal_MS <- coef(fitted_models_MS[["Lognormal"]])
se_lognormal_MS <- sqrt(diag(vcov(fitted_models_MS[["Lognormal"]])))

# Calculate 95% confidence intervals on the log-time scale
ci_low_lognormal_MS <- coef_lognormal_MS - 1.96 * se_lognormal_MS
ci_high_lognormal_MS <- coef_lognormal_MS + 1.96 * se_lognormal_MS

# Transform confidence intervals to the time scale
time_ratio_lower_lognormal_MS <- exp(ci_low_lognormal_MS)
time_ratio_upper_lognormal_MS <- exp(ci_high_lognormal_MS)

# Calculate time ratios
time_ratios_lognormal_MS <- exp(coef_lognormal_MS)

# Print results for Lognormal model
print("Time Ratios for Lognormal Model:")
print(time_ratios_lognormal_MS)
print("95% Confidence Intervals Lower Bounds:")
print(time_ratio_lower_lognormal_MS)
print("95% Confidence Intervals Upper Bounds:")
print(time_ratio_upper_lognormal_MS)
