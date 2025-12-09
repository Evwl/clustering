install.packages(c("survival", "survminer"))
library("survival")
library("survminer")
library("foreign")
library("tidyverse")
library("haven")
library("dplyr")

data<- read_sav ("/Users/evonwong/Library/CloudStorage/OneDrive-TheEducationUniversityofHongKong/Evon/UKBB/Clustering/[26 June Depress].sav")

# View the first few rows of the dataset
head(data)
# Summary of the dataset
summary(data)
# View the structure of the dataset
str(data)
#########################depression################################
selected_data_depre <- data %>%
  select(Depressive_dis_onset_end_death_time, Depressive_dis_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2,Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode)
  )
library(survival)
cox_depress_model <- coxph(Surv(Depressive_dis_onset_end_death_time,Depressive_dis_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_depre) 
summary (cox_depress_model)

# Example of running separate models for each income group
income_levels <- levels(selected_data_depre$Ave_Household_Income_mir_v2)
models_by_income <- lapply(income_levels, function(income) {
  data_subset <- filter(selected_data_depre, Ave_Household_Income_mir_v2 == income)
  coxph(Surv(Depressive_dis_onset_end_death_time, Depressive_dis_onset_end_death_0_1) ~ 
          Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Cluster_k4_v2_recode, 
        data = data_subset)
})

# Example of summarizing one of the models
summary(models_by_income[[1]])  # Replace 1 with appropriate index for specific income level
# Example of summarizing one of the models
summary(models_by_income[[2]])  # Replace 1 with appropriate index for specific income level



####### pattern 2 as reference group#########
selected_data_depre_ref2<- data %>%
  select(Depressive_dis_onset_end_death_time, Depressive_dis_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_depress_ref2_model <- coxph(Surv(Depressive_dis_onset_end_death_time, Depressive_dis_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_depre_ref2) 
summary (cox_depress_ref2_model)

####### pattern 3 as reference group#########
selected_data_depre_ref3<- data %>%
  select(Depressive_dis_onset_end_death_time, Depressive_dis_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_depress_ref3_model <- coxph(Surv(Depressive_dis_onset_end_death_time, Depressive_dis_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_depre_ref3) 
summary (cox_depress_ref3_model)

####### pattern 4 as reference group#########
selected_data_depre_ref4<- data %>%
  select(Depressive_dis_onset_end_death_time, Depressive_dis_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_depress_ref4_model <- coxph(Surv(Depressive_dis_onset_end_death_time,Depressive_dis_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_depre_ref4) 
summary (cox_depress_ref4_model)


################################depression F32########################
selected_data_depre_f32 <- data %>%
  select(Depres_dis_F32_onset_end_death_time, Depres_dis_F32_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode)
  )
library(survival)
cox_depress_F32_model <- coxph(Surv(Depres_dis_F32_onset_end_death_time, Depres_dis_F32_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_depre_f32) 
summary (cox_depress_F32_model)

####### pattern 2 as reference group#########
selected_data_depre_f32_ref2<- data %>%
  select(Depres_dis_F32_onset_end_death_time, Depres_dis_F32_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir = as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )

library(survival)
cox_depress_F32_ref2_model <- coxph(Surv(Depres_dis_F32_onset_end_death_time, Depres_dis_F32_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2  + Cluster_k4_v2_recode, data = selected_data_depre_f32_ref2) 
summary (cox_depress_F32_ref2_model)

####### pattern 3 as reference group#########
selected_data_depre_f32_ref3<- data %>%
  select(Depres_dis_F32_onset_end_death_time, Depres_dis_F32_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir = as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )

library(survival)
cox_depress_F32_ref3_model <- coxph(Surv(Depres_dis_F32_onset_end_death_time, Depres_dis_F32_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2  + Cluster_k4_v2_recode, data = selected_data_depre_f32_ref3) 
summary (cox_depress_F32_ref3_model)

####### pattern 4 as reference group#########
selected_data_depre_f32_ref4<- data %>%
  select(Depres_dis_F32_onset_end_death_time, Depres_dis_F32_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir = as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )

library(survival)
cox_depress_F32_ref4_model <- coxph(Surv(Depres_dis_F32_onset_end_death_time, Depres_dis_F32_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_depre_f32_ref4) 
summary (cox_depress_F32_ref4_model)


############################depression F33#############################
selected_data_depre_f33 <- data %>%
  select(Depres_dis_F33_onset_end_death_time, Depres_dis_F33_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode)
  )
library(survival)
cox_depress_F33_model <- coxph(Surv(Depres_dis_F33_onset_end_death_time, Depres_dis_F33_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_depre_f33) 
summary (cox_depress_F33_model)

#####pattern 2 as reference group ###
selected_data_depre_f33_ref2 <- data %>%
  select(Depres_dis_F33_onset_end_death_time, Depres_dis_F33_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode)%>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
cox_depress_F33_ref2_model <- coxph(Surv(Depres_dis_F33_onset_end_death_time, Depres_dis_F33_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_depre_f33_ref2) 
summary (cox_depress_F33_ref2_model)

###############pattern 3 as reference group ###
selected_data_depre_f33_ref3 <- data %>%
  select(Depres_dis_F33_onset_end_death_time, Depres_dis_F33_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode)%>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
cox_depress_F33_ref3_model <- coxph(Surv(Depres_dis_F33_onset_end_death_time, Depres_dis_F33_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_depre_f33_ref3) 
summary (cox_depress_F33_ref3_model)

###############pattern 4 as reference group ###
selected_data_depre_f33_ref4 <- data %>%
  select(Depres_dis_F33_onset_end_death_time, Depres_dis_F33_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode)%>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
cox_depress_F33_ref4_model <- coxph(Surv(Depres_dis_F33_onset_end_death_time, Depres_dis_F33_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_depre_f33_ref4) 
summary (cox_depress_F33_ref4_model)

#######################################################AFT model for depression###########################
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
fitted_models <- list()

# Loop to fit models and store results
for (i in names(models)) {
  fitted_models[[i]] <- survreg(Surv(Depressive_dis_onset_end_death_time, Depressive_dis_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_depre, dist = models[[i]])
}

# View summaries and calculate AIC/BIC
aic_values <- sapply(fitted_models, AIC)
bic_values <- sapply(fitted_models, BIC)

# Print summaries
lapply(fitted_models, summary)

# Print AIC and BIC values
print(aic_values)
print(bic_values)


# Calculate and display time ratios
time_ratios_depress <- exp(coef(fitted_models[["Weibull"]]))
print(time_ratios_depress)

# Calculate and display time ratios for the Weibull model
# Extract coefficients and their standard errors
coef_weibull_depress <- coef(fitted_models[["Weibull"]])
se_weibull_depress <- sqrt(diag(vcov(fitted_models[["Weibull"]])))

# Calculate 95% confidence intervals on the log-time scale
ci_low_weibull_depress <- coef_weibull_depress - 1.96 * se_weibull_depress
ci_high_weibull_depress <- coef_weibull_depress + 1.96 * se_weibull_depress

# Transform confidence intervals to the time scale
time_ratio_lower_weibull_depress <- exp(ci_low_weibull_depress)
time_ratio_upper_weibull_depress <- exp(ci_high_weibull_depress)

# Calculate time ratios
time_ratios_weibull_depress <- exp(coef_weibull_depress)

# Print results for Weibull model
print("Time Ratios for Weibull Model:")
print(time_ratios_weibull_depress)
print("95% Confidence Intervals Lower Bounds:")
print(time_ratio_lower_weibull_depress)
print("95% Confidence Intervals Upper Bounds:")
print(time_ratio_upper_weibull_depress)
