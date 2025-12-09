library("survival")
library("survminer")
library("foreign")
library("tidyverse")
library("haven")
library("dplyr")

data<- read_sav ("/Users/evonwong/Library/CloudStorage/OneDrive-TheEducationUniversityofHongKong/Evon/UKBB/Clustering/[26 June Arthritos].sav")
head(data)
# Summary of the dataset
summary(data)
# View the structure of the dataset
str(data)



selected_data_ARH <- data %>%
  select(Arthritos_onset_end_death_time, Arthritos_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode)
  )
library(survival)
cox_ARH_model <- coxph(Surv(Arthritos_onset_end_death_time, Arthritos_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_ARH) 
summary (cox_ARH_model)



# Example of running separate models for each income group
income_levels <- levels(selected_data_ARH$Ave_Household_Income_mir_v2)
models_by_income <- lapply(income_levels, function(income) {
  data_subset <- filter(selected_data_ARH, Ave_Household_Income_mir_v2 == income)
  coxph(Surv(Arthritos_onset_end_death_time, Arthritos_onset_end_death_0_1) ~ 
          Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Cluster_k4_v2_recode, 
        data = data_subset)
})

# Example of summarizing one of the models
summary(models_by_income[[1]])  # Replace 1 with appropriate index for specific income level
# Example of summarizing one of the models
summary(models_by_income[[2]])  # Replace 1 with appropriate index for specific income level



################arthritos ref2##########
selected_data_ARHref2 <- data %>%
  select(Arthritos_onset_end_death_time, Arthritos_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_ARHref2_model <- coxph(Surv(Arthritos_onset_end_death_time, Arthritos_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_ARHref2) 
summary (cox_ARHref2_model)

################arthritos ref3##########
selected_data_ARHref3 <- data %>%
  select(Arthritos_onset_end_death_time, Arthritos_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_ARHref3_model <- coxph(Surv(Arthritos_onset_end_death_time, Arthritos_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_ARHref3) 
summary (cox_ARHref3_model)

################arthritos ref4##########
selected_data_ARHref4 <- data %>%
  select(Arthritos_onset_end_death_time, Arthritos_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_ARHref4_model <- coxph(Surv(Arthritos_onset_end_death_time, Arthritos_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_ARHref4) 
summary (cox_ARHref4_model)


################Rheuma ref1##########
selected_data_rh <- data %>%
  select(RH_AR_onset_end_death_time, RH_AR_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode)
  )
library(survival)
cox_rh_model <- coxph(Surv(RH_AR_onset_end_death_time, RH_AR_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_rh) 
summary (cox_rh_model)
################Rheuma ref2##########
selected_data_rhref2 <- data %>%
  select(RH_AR_onset_end_death_time, RH_AR_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_rhref2_model <- coxph(Surv(RH_AR_onset_end_death_time, RH_AR_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_rhref2) 
summary (cox_rhref2_model)

################Rheuma ref3##########
selected_data_rhref3 <- data %>%
  select(RH_AR_onset_end_death_time, RH_AR_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_rhref3_model <- coxph(Surv(RH_AR_onset_end_death_time, RH_AR_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_rhref3) 
summary (cox_rhref3_model)

################Rheuma ref4##########
selected_data_rhref4 <- data %>%
  select(RH_AR_onset_end_death_time, RH_AR_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_rhref4_model <- coxph(Surv(RH_AR_onset_end_death_time, RH_AR_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_rhref4) 
summary (cox_rhref4_model)


##############################OA ###############
######OA ref1##########
selected_data_OA <- data %>%
  select(OA_onset_end_death_time, OA_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode)
  )
library(survival)
cox_OA_model <- coxph(Surv(OA_onset_end_death_time, OA_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_OA) 
summary (cox_OA_model)


###OA ref2#######
selected_data_OAref2 <- data %>%
  select(OA_onset_end_death_time, OA_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_OAref2_model <- coxph(Surv(OA_onset_end_death_time, OA_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_OAref2) 
summary (cox_OAref2_model)
###OA ref3#######
selected_data_OAref3 <- data %>%
  select(OA_onset_end_death_time, OA_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_OAref3_model <- coxph(Surv(OA_onset_end_death_time, OA_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_OAref3) 
summary (cox_OAref3_model)

###OA ref4#######
selected_data_OAref4 <- data %>%
  select(OA_onset_end_death_time, OA_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_OAref4_model <- coxph(Surv(OA_onset_end_death_time, OA_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_OAref4) 
summary (cox_OAref4_model)



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
fitted_models_ARH <- list()

# Loop to fit models and store results
for (i in names(models)) {
  fitted_models_ARH [[i]] <- survreg(Surv(Arthritos_onset_end_death_time, Arthritos_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_ARH, dist = models[[i]])
}

# View summaries and calculate AIC/BIC
aic_values_ARH <- sapply(fitted_models_ARH , AIC)
bic_values_ARH <- sapply(fitted_models_ARH , BIC)

# Print summaries
lapply(fitted_models_ARH , summary)

# Print AIC and BIC values
print(aic_values_ARH)
print(bic_values_ARH)


# Calculate and display time ratios
time_ratios_ARH <- exp(coef(fitted_models_ARH [["Weibull"]]))
print(time_ratios_ARH)

# Calculate and display time ratios for the Weibull model
# Extract coefficients and their standard errors
coef_Weibull_ARH <- coef(fitted_models_ARH[["Weibull"]])
se_Weibull_ARH <- sqrt(diag(vcov(fitted_models_ARH[["Weibull"]])))

# Calculate 95% confidence intervals on the log-time scale
ci_low_Weibull_ARH <- coef_Weibull_ARH - 1.96 * se_Weibull_ARH
ci_high_Weibull_ARH <- coef_Weibull_ARH + 1.96 * se_Weibull_ARH

# Transform confidence intervals to the time scale
time_ratio_lower_Weibull_ARH <- exp(ci_low_Weibull_ARH)
time_ratio_upper_Weibull_ARH <- exp(ci_high_Weibull_ARH)

# Calculate time ratios
time_ratios_Weibull_ARH <- exp(coef_Weibull_ARH)

# Print results for Weibull model
print("Time Ratios for Weibull Model:")
print(time_ratios_Weibull_ARH)
print("95% Confidence Intervals Lower Bounds:")
print(time_ratio_lower_Weibull_ARH)
print("95% Confidence Intervals Upper Bounds:")
print(time_ratio_upper_Weibull_ARH)
