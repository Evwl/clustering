install.packages(c("survival", "survminer"))
library("survival")
library("survminer")
library("foreign")
library("tidyverse")
library("haven")
library("dplyr")

data<- read_sav ("/Users/evonwong/Library/CloudStorage/OneDrive-TheEducationUniversityofHongKong/Evon/UKBB/Clustering/[26 June Dementia].sav")

# View the first few rows of the dataset
head(data)
# Summary of the dataset
summary(data)
# View the structure of the dataset
str(data)

######ref1#####
selected_data_demen <- data %>%
  select(Dementia_dis_onset_end_death_time, Dementia_dis_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode)
  )
library(survival)
cox_demen_model <- coxph(Surv(Dementia_dis_onset_end_death_time, Dementia_dis_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_demen) 
summary (cox_demen_model)

# Example of running separate models for each income group
income_levels <- levels(selected_data_demen$Ave_Household_Income_mir_v2)
models_by_income <- lapply(income_levels, function(income) {
  data_subset <- filter(selected_data_demen, Ave_Household_Income_mir_v2 == income)
  coxph(Surv(Dementia_dis_onset_end_death_time, Dementia_dis_onset_end_death_0_1) ~ 
          Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Cluster_k4_v2_recode, 
        data = data_subset)
})

# Example of summarizing one of the models
summary(models_by_income[[1]])  # Replace 1 with appropriate index for specific income level
# Example of summarizing one of the models
summary(models_by_income[[2]])  # Replace 1 with appropriate index for specific income level


######ref2#####
selected_data_demen_ref2 <- data %>%
  select(Dementia_dis_onset_end_death_time, Dementia_dis_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_demen_ref2_model <- coxph(Surv(Dementia_dis_onset_end_death_time, Dementia_dis_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_demen_ref2) 
summary (cox_demen_ref2_model)
######ref3#####
selected_data_demen_ref3 <- data %>%
  select(Dementia_dis_onset_end_death_time, Dementia_dis_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_demen_ref3_model <- coxph(Surv(Dementia_dis_onset_end_death_time, Dementia_dis_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_demen_ref3) 
summary (cox_demen_ref3_model)

######ref4#####
selected_data_demen_ref4 <- data %>%
  select(Dementia_dis_onset_end_death_time, Dementia_dis_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_demen_ref4_model <- coxph(Surv(Dementia_dis_onset_end_death_time, Dementia_dis_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_demen_ref4) 
summary (cox_demen_ref4_model)


##################DEmentia F00##############################33
###ref1
selected_data_demenF00_ref1 <- data %>%
  select(Dementia_dis_F00_onset_end_death_time, Dementia_dis_F00_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_demenF00_ref1_model <- coxph(Surv(Dementia_dis_F00_onset_end_death_time, Dementia_dis_F00_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_demenF00_ref1) 
summary (cox_demenF00_ref1_model)

###ref2
selected_data_demenF00_ref2 <- data %>%
  select(Dementia_dis_F00_onset_end_death_time, Dementia_dis_F00_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_demenF00_ref2_model <- coxph(Surv(Dementia_dis_F00_onset_end_death_time, Dementia_dis_F00_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_demenF00_ref2) 
summary (cox_demenF00_ref2_model)

###ref3
selected_data_demenF00_ref3 <- data %>%
  select(Dementia_dis_F00_onset_end_death_time, Dementia_dis_F00_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_demenF00_ref3_model <- coxph(Surv(Dementia_dis_F00_onset_end_death_time, Dementia_dis_F00_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_demenF00_ref3) 
summary (cox_demenF00_ref3_model)

###ref4
selected_data_demenF00_ref4 <- data %>%
  select(Dementia_dis_F00_onset_end_death_time, Dementia_dis_F00_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_demenF00_ref4_model <- coxph(Surv(Dementia_dis_F00_onset_end_death_time, Dementia_dis_F00_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_demenF00_ref4) 
summary (cox_demenF00_ref4_model)

##########################F01#########
#######ref1
selected_data_demenF01_ref1 <- data %>%
  select(Dementia_dis_F01_onset_end_death_time, Dementia_dis_F01_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_demenF01_ref1_model <- coxph(Surv(Dementia_dis_F01_onset_end_death_time, Dementia_dis_F01_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_demenF01_ref1) 
summary (cox_demenF01_ref1_model)

#######ref2
selected_data_demenF01_ref2 <- data %>%
  select(Dementia_dis_F01_onset_end_death_time, Dementia_dis_F01_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_demenF01_ref2_model <- coxph(Surv(Dementia_dis_F01_onset_end_death_time, Dementia_dis_F01_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_demenF01_ref2) 
summary (cox_demenF01_ref2_model)

#######ref3
selected_data_demenF01_ref3 <- data %>%
  select(Dementia_dis_F01_onset_end_death_time, Dementia_dis_F01_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_demenF01_ref3_model <- coxph(Surv(Dementia_dis_F01_onset_end_death_time, Dementia_dis_F01_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_demenF01_ref3) 
summary (cox_demenF01_ref3_model)

#######ref4
selected_data_demenF01_ref4 <- data %>%
  select(Dementia_dis_F01_onset_end_death_time, Dementia_dis_F01_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_demenF01_ref4_model <- coxph(Surv(Dementia_dis_F01_onset_end_death_time, Dementia_dis_F01_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_demenF01_ref4) 
summary (cox_demenF01_ref4_model)

###################F02
###ref1
selected_data_demenF02_ref1 <- data %>%
  select(Dementia_dis_F02_onset_end_death_time, Dementia_dis_F02_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_demenF02_ref1_model <- coxph(Surv(Dementia_dis_F02_onset_end_death_time, Dementia_dis_F02_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_demenF02_ref1) 
summary (cox_demenF02_ref1_model)
###ref2
selected_data_demenF02_ref2 <- data %>%
  select(Dementia_dis_F02_onset_end_death_time, Dementia_dis_F02_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_demenF02_ref2_model <- coxph(Surv(Dementia_dis_F02_onset_end_death_time, Dementia_dis_F02_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_demenF02_ref2) 
summary (cox_demenF02_ref2_model)




###ref3
selected_data_demenF02_ref3 <- data %>%
  select(Dementia_dis_F02_onset_end_death_time, Dementia_dis_F02_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_demenF02_ref3_model <- coxph(Surv(Dementia_dis_F02_onset_end_death_time, Dementia_dis_F02_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_demenF02_ref3) 
summary (cox_demenF02_ref3_model)

###ref4
selected_data_demenF02_ref4 <- data %>%
  select(Dementia_dis_F02_onset_end_death_time, Dementia_dis_F02_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_demenF02_ref4_model <- coxph(Surv(Dementia_dis_F02_onset_end_death_time, Dementia_dis_F02_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_demenF02_ref4) 
summary (cox_demenF02_ref4_model)
####################################F03####################
###ref1
selected_data_demenF03_ref1 <- data %>%
  select(Dementia_dis_F03_onset_end_death_time, Dementia_dis_F03_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_demenF03_ref1_model <- coxph(Surv(Dementia_dis_F03_onset_end_death_time, Dementia_dis_F03_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_demenF03_ref1) 
summary (cox_demenF03_ref1_model)
###ref2
selected_data_demenF03_ref2 <- data %>%
  select(Dementia_dis_F03_onset_end_death_time, Dementia_dis_F03_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_demenF03_ref2_model <- coxph(Surv(Dementia_dis_F03_onset_end_death_time, Dementia_dis_F03_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_demenF03_ref2) 
summary (cox_demenF03_ref2_model)

###ref3
selected_data_demenF03_ref3 <- data %>%
  select(Dementia_dis_F03_onset_end_death_time, Dementia_dis_F03_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_demenF03_ref3_model <- coxph(Surv(Dementia_dis_F03_onset_end_death_time, Dementia_dis_F03_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_demenF03_ref3) 
summary (cox_demenF03_ref3_model)

###ref4
selected_data_demenF03_ref4 <- data %>%
  select(Dementia_dis_F03_onset_end_death_time, Dementia_dis_F03_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_demenF03_ref4_model <- coxph(Surv(Dementia_dis_F03_onset_end_death_time, Dementia_dis_F03_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_demenF03_ref4) 
summary (cox_demenF03_ref4_model)

###################AFT Model
###F00-F03

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
fitted_models_Dementia <- list()

# Loop to fit models and store results
for (i in names(models)) {
  fitted_models_Dementia [[i]] <- survreg(Surv(Dementia_dis_onset_end_death_time, Dementia_dis_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_demen, dist = models[[i]])
}

# View summaries and calculate AIC/BIC
aic_values_Dementia <- sapply(fitted_models_Dementia , AIC)
bic_values_Dementia <- sapply(fitted_models_Dementia , BIC)

# Print summaries
lapply(fitted_models_Dementia , summary)

# Print AIC and BIC values
print(aic_values_Dementia)
print(bic_values_Dementia)


# Calculate and display time ratios
time_ratios_dementia <- exp(coef(fitted_models_Dementia [["Weibull"]]))
print(time_ratios_dementia)

# Calculate and display time ratios for the Weibull model
# Extract coefficients and their standard errors
coef_weibull_dementia <- coef(fitted_models_Dementia[["Weibull"]])
se_weibull_dementia <- sqrt(diag(vcov(fitted_models_Dementia[["Weibull"]])))

# Calculate 95% confidence intervals on the log-time scale
ci_low_weibull_dementia <- coef_weibull_dementia - 1.96 * se_weibull_dementia
ci_high_weibull_dementia <- coef_weibull_dementia + 1.96 * se_weibull_dementia

# Transform confidence intervals to the time scale
time_ratio_lower_weibull_dementia <- exp(ci_low_weibull_dementia)
time_ratio_upper_weibull_dementia <- exp(ci_high_weibull_dementia)

# Calculate time ratios
time_ratios_weibull_dementia <- exp(coef_weibull_dementia)

# Print results for Weibull model
print("Time Ratios for Weibull Model:")
print(time_ratios_weibull_dementia)
print("95% Confidence Intervals Lower Bounds:")
print(time_ratio_lower_weibull_dementia)
print("95% Confidence Intervals Upper Bounds:")
print(time_ratio_upper_weibull_dementia)
