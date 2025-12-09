install.packages(c("survival", "survminer"))
library("survival")
library("survminer")
library("foreign")
library("tidyverse")
library("haven")
library("dplyr")

data<- read_sav ("/Users/evonwong/Library/CloudStorage/OneDrive-TheEducationUniversityofHongKong/Evon/UKBB/Clustering/[26 June Anxiety].sav")

# View the first few rows of the dataset
head(data)
# Summary of the dataset
summary(data)
# View the structure of the dataset
str(data)

#########################anxiety################################
selected_data_Anx <- data %>%
  select(Anxiety_dis_onset_end_death_time, Anxiety_dis_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode)
  )
library(survival)
cox_anx_model <- coxph(Surv(Anxiety_dis_onset_end_death_time, Anxiety_dis_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_Anx) 
summary (cox_anx_model)

# Example of running separate models for each income group
income_levels <- levels(selected_data_Anx$Ave_Household_Income_mir_v2)
models_by_income <- lapply(income_levels, function(income) {
  data_subset <- filter(selected_data_Anx, Ave_Household_Income_mir_v2 == income)
  coxph(Surv(Anxiety_dis_onset_end_death_time, Anxiety_dis_onset_end_death_0_1) ~ 
          Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Cluster_k4_v2_recode, 
        data = data_subset)
})

# Example of summarizing one of the models
summary(models_by_income[[1]])  # Replace 1 with appropriate index for specific income level
# Example of summarizing one of the models
summary(models_by_income[[2]])  # Replace 1 with appropriate index for specific income level

#######ref group pattern 2###
selected_data_Anx_ref2 <- data %>%
  select(Anxiety_dis_onset_end_death_time, Anxiety_dis_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_anx_model_ref2 <- coxph(Surv(Anxiety_dis_onset_end_death_time, Anxiety_dis_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_Anx_ref2) 
summary (cox_anx_model_ref2)

####ref 3
selected_data_Anx_ref3 <- data %>%
  select(Anxiety_dis_onset_end_death_time, Anxiety_dis_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_anx_model_ref3 <- coxph(Surv(Anxiety_dis_onset_end_death_time, Anxiety_dis_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_Anx_ref3) 
summary (cox_anx_model_ref3)

####ref 4
selected_data_Anx_ref4 <- data %>%
  select(Anxiety_dis_onset_end_death_time, Anxiety_dis_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_anx_model_ref4 <- coxph(Surv(Anxiety_dis_onset_end_death_time, Anxiety_dis_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_Anx_ref4) 
summary (cox_anx_model_ref4)
##########################################################################
################################Anxiety F40##############################################3
selected_data_AnxF40 <- data %>%
  select(Anxiety_dis_F40_onset_end_death_time, Anxiety_dis_F40_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode)
  )
library(survival)
cox_anxF40_model <- coxph(Surv(Anxiety_dis_F40_onset_end_death_time, Anxiety_dis_F40_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_AnxF40) 
summary (cox_anxF40_model)
###########ref2###########
selected_data_AnxF40_ref2 <- data %>%
  select(Anxiety_dis_F40_onset_end_death_time, Anxiety_dis_F40_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_anxF40_ref2_model <- coxph(Surv(Anxiety_dis_F40_onset_end_death_time, Anxiety_dis_F40_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_AnxF40_ref2) 
summary (cox_anxF40_ref2_model)
###########ref3###########
selected_data_AnxF40_ref3 <- data %>%
  select(Anxiety_dis_F40_onset_end_death_time, Anxiety_dis_F40_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_anxF40_ref3_model <- coxph(Surv(Anxiety_dis_F40_onset_end_death_time, Anxiety_dis_F40_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_AnxF40_ref3) 
summary (cox_anxF40_ref3_model)
###########ref4###########
selected_data_AnxF40_ref4 <- data %>%
  select(Anxiety_dis_F40_onset_end_death_time, Anxiety_dis_F40_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_anxF40_ref4_model <- coxph(Surv(Anxiety_dis_F40_onset_end_death_time, Anxiety_dis_F40_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_AnxF40_ref4) 
summary (cox_anxF40_ref4_model)

################################Anxiety F41##############################################3
selected_data_AnxF41 <- data %>%
  select(Anxiety_dis_F41_onset_end_death_time, Anxiety_dis_F41_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode)
  )
library(survival)
cox_anxF41_model <- coxph(Surv(Anxiety_dis_F41_onset_end_death_time, Anxiety_dis_F41_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_AnxF41) 
summary (cox_anxF41_model)
#########################ref2###########
selected_data_AnxF41_ref2 <- data %>%
  select(Anxiety_dis_F41_onset_end_death_time, Anxiety_dis_F41_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_anxF41_ref2_model <- coxph(Surv(Anxiety_dis_F41_onset_end_death_time, Anxiety_dis_F41_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_AnxF41_ref2) 
summary (cox_anxF41_ref2_model)
#########################ref3###########
selected_data_AnxF41_ref3 <- data %>%
  select(Anxiety_dis_F41_onset_end_death_time, Anxiety_dis_F41_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_anxF41_ref3_model <- coxph(Surv(Anxiety_dis_F41_onset_end_death_time, Anxiety_dis_F41_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_AnxF41_ref3) 
summary (cox_anxF41_ref3_model)
#########################ref4###########
selected_data_AnxF41_ref4 <- data %>%
  select(Anxiety_dis_F41_onset_end_death_time, Anxiety_dis_F41_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_anxF41_ref4_model <- coxph(Surv(Anxiety_dis_F41_onset_end_death_time, Anxiety_dis_F41_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_AnxF41_ref4) 
summary (cox_anxF41_ref4_model)


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
fitted_models_anxiety <- list()

# Loop to fit models and store results
for (i in names(models)) {
  fitted_models_anxiety [[i]] <- survreg(Surv(Anxiety_dis_onset_end_death_time, Anxiety_dis_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_Anx, dist = models[[i]])
}

# View summaries and calculate AIC/BIC
aic_values_anxiety <- sapply(fitted_models_anxiety , AIC)
bic_values_anxiety <- sapply(fitted_models_anxiety , BIC)

# Print summaries
lapply(fitted_models_anxiety , summary)

# Print AIC and BIC values
print(aic_values_anxiety)
print(bic_values_anxiety)


# Calculate and display time ratios
time_ratios_anxiety <- exp(coef(fitted_models_anxiety [["Weibull"]]))
print(time_ratios_anxiety)

# Calculate and display time ratios for the Weibull model
# Extract coefficients and their standard errors
coef_weibull_anxiety <- coef(fitted_models_anxiety[["Weibull"]])
se_weibull_anxiety <- sqrt(diag(vcov(fitted_models_anxiety[["Weibull"]])))

# Calculate 95% confidence intervals on the log-time scale
ci_low_weibull_anxiety <- coef_weibull_anxiety - 1.96 * se_weibull_anxiety
ci_high_weibull_anxiety <- coef_weibull_anxiety + 1.96 * se_weibull_anxiety

# Transform confidence intervals to the time scale
time_ratio_lower_weibull_anxiety <- exp(ci_low_weibull_anxiety)
time_ratio_upper_weibull_anxiety <- exp(ci_high_weibull_anxiety)

# Calculate time ratios
time_ratios_weibull_anxiety <- exp(coef_weibull_anxiety)

# Print results for Weibull model
print("Time Ratios for Weibull Model:")
print(time_ratios_weibull_anxiety)
print("95% Confidence Intervals Lower Bounds:")
print(time_ratio_lower_weibull_anxiety)
print("95% Confidence Intervals Upper Bounds:")
print(time_ratio_upper_weibull_anxiety)




