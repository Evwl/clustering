install.packages(c("survival", "survminer"))
library("survival")
library("survminer")
library("foreign")
library("tidyverse")
library("haven")
library("dplyr")


data<- read_sav ("/Users/evonwong/Library/CloudStorage/OneDrive-TheEducationUniversityofHongKong/Evon/UKBB/Clustering/[26 June chronic pain].sav")
# View the first few rows of the dataset
head(data)
# Summary of the dataset
summary(data)
# View the structure of the dataset
str(data)

###########chronic pain
############################ref1#########
selected_data_chronicpain_ref1 <- data %>%
  select(Chronic_pain_onset_end_death_time, Chronic_pain_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_chronicpain_ref1_model <- coxph(Surv(Chronic_pain_onset_end_death_time, Chronic_pain_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_chronicpain_ref1) 
summary (cox_chronicpain_ref1_model)

# Example of running separate models for each income group
income_levels <- levels(selected_data_chronicpain_ref1$Ave_Household_Income_mir_v2)
models_by_income <- lapply(income_levels, function(income) {
  data_subset <- filter(selected_data_chronicpain_ref1, Ave_Household_Income_mir_v2 == income)
  coxph(Surv(Chronic_pain_onset_end_death_time, Chronic_pain_onset_end_death_0_1) ~ 
          Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Cluster_k4_v2_recode, 
        data = data_subset)
})

# Example of summarizing one of the models
summary(models_by_income[[1]])  # Replace 1 with appropriate index for specific income level
# Example of summarizing one of the models
summary(models_by_income[[2]])  # Replace 1 with appropriate index for specific income level


########################ref2################
selected_data_chronicpain_ref2 <- data %>%
  select(Chronic_pain_onset_end_death_time, Chronic_pain_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_chronicpain_ref2_model <- coxph(Surv(Chronic_pain_onset_end_death_time, Chronic_pain_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_chronicpain_ref2) 
summary (cox_chronicpain_ref2_model)
########################ref3################
selected_data_chronicpain_ref3 <- data %>%
  select(Chronic_pain_onset_end_death_time, Chronic_pain_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_chronicpain_ref3_model <- coxph(Surv(Chronic_pain_onset_end_death_time, Chronic_pain_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_chronicpain_ref3) 
summary (cox_chronicpain_ref3_model)

########################ref4################
selected_data_chronicpain_ref4 <- data %>%
  select(Chronic_pain_onset_end_death_time, Chronic_pain_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_chronicpain_ref4_model <- coxph(Surv(Chronic_pain_onset_end_death_time, Chronic_pain_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_chronicpain_ref4) 
summary (cox_chronicpain_ref4_model)

#############################Other chronic pain R52.2
selected_data_R522chronicpain_ref1 <- data %>%
  select(Chronic_pain_other_R522_onset_end_death_time, Chronic_pain_other_R522_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_R522chronicpain_ref1_model <- coxph(Surv(Chronic_pain_other_R522_onset_end_death_time, Chronic_pain_other_R522_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_R522chronicpain_ref1) 
summary (cox_R522chronicpain_ref1_model)

######################REF2
selected_data_R522chronicpain_ref2 <- data %>%
  select(Chronic_pain_other_R522_onset_end_death_time, Chronic_pain_other_R522_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_R522chronicpain_ref2_model <- coxph(Surv(Chronic_pain_other_R522_onset_end_death_time, Chronic_pain_other_R522_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_R522chronicpain_ref2) 
summary (cox_R522chronicpain_ref2_model)

######################ref3
selected_data_R522chronicpain_ref3 <- data %>%
  select(Chronic_pain_other_R522_onset_end_death_time, Chronic_pain_other_R522_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_R522chronicpain_ref3_model <- coxph(Surv(Chronic_pain_other_R522_onset_end_death_time, Chronic_pain_other_R522_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_R522chronicpain_ref3) 
summary (cox_R522chronicpain_ref3_model)

######################ref4
selected_data_R522chronicpain_ref4 <- data %>%
  select(Chronic_pain_other_R522_onset_end_death_time, Chronic_pain_other_R522_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_R522chronicpain_ref4_model <- coxph(Surv(Chronic_pain_other_R522_onset_end_death_time, Chronic_pain_other_R522_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_R522chronicpain_ref4) 
summary (cox_R522chronicpain_ref4_model)


#####################################Low back pain
###ref1

selected_data_M545chronicpain_ref1 <- data %>%
  select(Chronic_pain_back_M545_onset_end_death_time, Chronic_pain_back_M545_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_M545chronicpain_ref1_model <- coxph(Surv(Chronic_pain_back_M545_onset_end_death_time, Chronic_pain_back_M545_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_M545chronicpain_ref1) 
summary (cox_M545chronicpain_ref1_model)

###ref2

selected_data_M545chronicpain_ref2 <- data %>%
  select(Chronic_pain_back_M545_onset_end_death_time, Chronic_pain_back_M545_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_M545chronicpain_ref2_model <- coxph(Surv(Chronic_pain_back_M545_onset_end_death_time, Chronic_pain_back_M545_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_M545chronicpain_ref2) 
summary (cox_M545chronicpain_ref2_model)

###ref3

selected_data_M545chronicpain_ref3 <- data %>%
  select(Chronic_pain_back_M545_onset_end_death_time, Chronic_pain_back_M545_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_M545chronicpain_ref3_model <- coxph(Surv(Chronic_pain_back_M545_onset_end_death_time, Chronic_pain_back_M545_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_M545chronicpain_ref3) 
summary (cox_M545chronicpain_ref3_model)

###ref4

selected_data_M545chronicpain_ref4 <- data %>%
  select(Chronic_pain_back_M545_onset_end_death_time, Chronic_pain_back_M545_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_M545chronicpain_ref4_model <- coxph(Surv(Chronic_pain_back_M545_onset_end_death_time, Chronic_pain_back_M545_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_M545chronicpain_ref4) 
summary (cox_M545chronicpain_ref4_model)


#######################limb pain M796
###ref1
selected_data_M796chronicpain_ref1 <- data %>%
  select(Chronic_pain_limb_M796_onset_end_death_time, Chronic_pain_limb_M796_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_M796chronicpain_ref1_model <- coxph(Surv(Chronic_pain_limb_M796_onset_end_death_time, Chronic_pain_limb_M796_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_M796chronicpain_ref1) 
summary (cox_M796chronicpain_ref1_model)

###ref2
selected_data_M796chronicpain_ref2 <- data %>%
  select(Chronic_pain_limb_M796_onset_end_death_time, Chronic_pain_limb_M796_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_M796chronicpain_ref2_model <- coxph(Surv(Chronic_pain_limb_M796_onset_end_death_time, Chronic_pain_limb_M796_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_M796chronicpain_ref2) 
summary (cox_M796chronicpain_ref2_model)

###ref3
selected_data_M796chronicpain_ref3 <- data %>%
  select(Chronic_pain_limb_M796_onset_end_death_time, Chronic_pain_limb_M796_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_M796chronicpain_ref3_model <- coxph(Surv(Chronic_pain_limb_M796_onset_end_death_time, Chronic_pain_limb_M796_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_M796chronicpain_ref3) 
summary (cox_M796chronicpain_ref3_model)

###ref4
selected_data_M796chronicpain_ref4 <- data %>%
  select(Chronic_pain_limb_M796_onset_end_death_time, Chronic_pain_limb_M796_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_M796chronicpain_ref4_model <- coxph(Surv(Chronic_pain_limb_M796_onset_end_death_time, Chronic_pain_limb_M796_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_M796chronicpain_ref4) 
summary (cox_M796chronicpain_ref4_model)


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
fitted_models_Chronic_pain <- list()

# Loop to fit models and store results
for (i in names(models)) {
  fitted_models_Chronic_pain [[i]] <- survreg(Surv(Chronic_pain_onset_end_death_time, Chronic_pain_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_chronicpain_ref1 , dist = models[[i]])
}

# View summaries and calculate AIC/BIC
aic_values_Chronic_pain <- sapply(fitted_models_Chronic_pain , AIC)
bic_values_Chronic_pain <- sapply(fitted_models_Chronic_pain , BIC)

# Print summaries
lapply(fitted_models_Chronic_pain , summary)

# Print AIC and BIC values
print(aic_values_Chronic_pain)
print(bic_values_Chronic_pain)


# Calculate and display time ratios
time_ratios_Chronic_pain <- exp(coef(fitted_models_Chronic_pain [["Lognormal"]]))
print(time_ratios_Chronic_pain)

# Calculate and display time ratios for the Lognormal model
# Extract coefficients and their standard errors
coef_lognormal_Chronic_pain <- coef(fitted_models_Chronic_pain[["Lognormal"]])
se_lognormal_Chronic_pain <- sqrt(diag(vcov(fitted_models_Chronic_pain[["Lognormal"]])))

# Calculate 95% confidence intervals on the log-time scale
ci_low_lognormal_Chronic_pain <- coef_lognormal_Chronic_pain - 1.96 * se_lognormal_Chronic_pain
ci_high_lognormal_Chronic_pain <- coef_lognormal_Chronic_pain + 1.96 * se_lognormal_Chronic_pain

# Transform confidence intervals to the time scale
time_ratio_lower_lognormal_Chronic_pain <- exp(ci_low_lognormal_Chronic_pain)
time_ratio_upper_lognormal_Chronic_pain <- exp(ci_high_lognormal_Chronic_pain)

# Calculate time ratios
time_ratios_lognormal_Chronic_pain <- exp(coef_lognormal_Chronic_pain)

# Print results for Lognormal model
print("Time Ratios for Lognormal Model:")
print(time_ratios_lognormal_Chronic_pain)
print("95% Confidence Intervals Lower Bounds:")
print(time_ratio_lower_lognormal_Chronic_pain)
print("95% Confidence Intervals Upper Bounds:")
print(time_ratio_upper_lognormal_Chronic_pain)
