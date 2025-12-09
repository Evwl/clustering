install.packages(c("survival", "survminer"))
library("survival")
library("survminer")
library("foreign")
library("tidyverse")
library("haven")
library("dplyr")

data<- read_sav ("/Users/evonwong/Library/CloudStorage/OneDrive-TheEducationUniversityofHongKong/Evon/UKBB/Clustering/[26 June schizo].sav")
# View the first few rows of the dataset
head(data)
# Summary of the dataset
summary(data)
# View the structure of the dataset
str(data)

################Schizophrenia ref1##########
selected_data_Sch <- data %>%
  select(Schz_delu_dis_F20_F25_onset_end_death_time, Schz_delu_dis_F20_F25_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode)
  )
library(survival)
cox_Sch_model <- coxph(Surv(Schz_delu_dis_F20_F25_onset_end_death_time, Schz_delu_dis_F20_F25_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_Sch) 
summary (cox_Sch_model)


# Example of running separate models for each income group
income_levels <- levels(selected_data_Sch$Ave_Household_Income_mir_v2)
models_by_income <- lapply(income_levels, function(income) {
  data_subset <- filter(selected_data_Sch, Ave_Household_Income_mir_v2 == income)
  coxph(Surv(Schz_delu_dis_F20_F25_onset_end_death_time, Schz_delu_dis_F20_F25_onset_end_death_0_1) ~ 
          Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Cluster_k4_v2_recode, 
        data = data_subset)
})

# Example of summarizing one of the models
summary(models_by_income[[1]])  # Replace 1 with appropriate index for specific income level
# Example of summarizing one of the models
summary(models_by_income[[2]])  # Replace 1 with appropriate index for specific income level

################Schizophrenia ref2##########
selected_data_Schref2 <- data %>%
  select(Schz_delu_dis_F20_F25_onset_end_death_time, Schz_delu_dis_F20_F25_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_Schref2_model <- coxph(Surv(Schz_delu_dis_F20_F25_onset_end_death_time, Schz_delu_dis_F20_F25_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 +  Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_Schref2) 
summary (cox_Schref2_model)
################Schizophrenia ref3##########
selected_data_Schref3 <- data %>%
  select(Schz_delu_dis_F20_F25_onset_end_death_time, Schz_delu_dis_F20_F25_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_Schref3_model <- coxph(Surv(Schz_delu_dis_F20_F25_onset_end_death_time, Schz_delu_dis_F20_F25_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 +  Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_Schref3) 
summary (cox_Schref3_model)
################Schizophrenia ref4##########
selected_data_Schref4 <- data %>%
  select(Schz_delu_dis_F20_F25_onset_end_death_time, Schz_delu_dis_F20_F25_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_Schref4_model <- coxph(Surv(Schz_delu_dis_F20_F25_onset_end_death_time, Schz_delu_dis_F20_F25_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 +  Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_Schref4) 
summary (cox_Schref4_model)

########################################schizophrenia delusion F20##################
######ref1

selected_data_SchF20ref1 <- data %>%
  select(Schz_delu_dis_F20_onset_end_death_time, Schz_delu_dis_F20_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_SchF20ref1_model <- coxph(Surv(Schz_delu_dis_F20_onset_end_death_time, Schz_delu_dis_F20_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 +  Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_SchF20ref1) 
summary (cox_SchF20ref1_model)

######ref2
selected_data_SchF20ref2 <- data %>%
  select(Schz_delu_dis_F20_onset_end_death_time, Schz_delu_dis_F20_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_SchF20ref2_model <- coxph(Surv(Schz_delu_dis_F20_onset_end_death_time, Schz_delu_dis_F20_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_SchF20ref2) 
summary (cox_SchF20ref2_model)

######ref3
selected_data_SchF20ref3 <- data %>%
  select(Schz_delu_dis_F20_onset_end_death_time, Schz_delu_dis_F20_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_SchF20ref3_model <- coxph(Surv(Schz_delu_dis_F20_onset_end_death_time, Schz_delu_dis_F20_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 +  Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_SchF20ref3) 
summary (cox_SchF20ref3_model)

######ref4
selected_data_SchF20ref4 <- data %>%
  select(Schz_delu_dis_F20_onset_end_death_time, Schz_delu_dis_F20_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_SchF20ref4_model <- coxph(Surv(Schz_delu_dis_F20_onset_end_death_time, Schz_delu_dis_F20_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 +  Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_SchF20ref4) 
summary (cox_SchF20ref4_model)

##########################################F21##################
####ref1

selected_data_SchF21ref1 <- data %>%
  select(Schz_delu_dis_F21_onset_end_death_time, Schz_delu_dis_F21_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_SchF21ref1_model <- coxph(Surv(Schz_delu_dis_F21_onset_end_death_time, Schz_delu_dis_F21_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_SchF21ref1) 
summary (cox_SchF21ref1_model)

####ref2

selected_data_SchF21ref2 <- data %>%
  select(Schz_delu_dis_F21_onset_end_death_time, Schz_delu_dis_F21_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_SchF21ref2_model <- coxph(Surv(Schz_delu_dis_F21_onset_end_death_time, Schz_delu_dis_F21_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 +  Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_SchF21ref2) 
summary (cox_SchF21ref2_model)


####ref3

selected_data_SchF21ref3 <- data %>%
  select(Schz_delu_dis_F21_onset_end_death_time, Schz_delu_dis_F21_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_SchF21ref3_model <- coxph(Surv(Schz_delu_dis_F21_onset_end_death_time, Schz_delu_dis_F21_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 +  Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_SchF21ref3) 
summary (cox_SchF21ref3_model)

####ref4
selected_data_SchF21ref4 <- data %>%
  select(Schz_delu_dis_F21_onset_end_death_time, Schz_delu_dis_F21_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_SchF21ref4_model <- coxph(Surv(Schz_delu_dis_F21_onset_end_death_time, Schz_delu_dis_F21_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 +  Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_SchF21ref4) 
summary (cox_SchF21ref4_model)


##########################################F22-24##################
####ref1
selected_data_SchF2224ref1 <- data %>%
  select(Schz_delu_dis_F22_F24_onset_end_death_time, Schz_delu_dis_F22_F24_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_SchF2224ref1_model <- coxph(Surv(Schz_delu_dis_F22_F24_onset_end_death_time, Schz_delu_dis_F22_F24_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 +  Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_SchF2224ref1) 
summary (cox_SchF2224ref1_model)

####ref2
selected_data_SchF2224ref2 <- data %>%
  select(Schz_delu_dis_F22_F24_onset_end_death_time, Schz_delu_dis_F22_F24_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_SchF2224ref2_model <- coxph(Surv(Schz_delu_dis_F22_F24_onset_end_death_time, Schz_delu_dis_F22_F24_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 +  Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_SchF2224ref2) 
summary (cox_SchF2224ref2_model)

####ref3
selected_data_SchF2224ref3 <- data %>%
  select(Schz_delu_dis_F22_F24_onset_end_death_time, Schz_delu_dis_F22_F24_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_SchF2224ref3_model <- coxph(Surv(Schz_delu_dis_F22_F24_onset_end_death_time, Schz_delu_dis_F22_F24_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 +  Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_SchF2224ref3) 
summary (cox_SchF2224ref3_model)

####ref4
selected_data_SchF2224ref4 <- data %>%
  select(Schz_delu_dis_F22_F24_onset_end_death_time, Schz_delu_dis_F22_F24_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_SchF2224ref4_model <- coxph(Surv(Schz_delu_dis_F22_F24_onset_end_death_time, Schz_delu_dis_F22_F24_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 +  Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_SchF2224ref4) 
summary (cox_SchF2224ref4_model)


##########################################F25##################
####ref1
selected_data_SchF25ref1 <- data %>%
  select(Schz_delu_dis_F25_onset_end_death_time, Schz_delu_dis_F25_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_SchF25ref1_model <- coxph(Surv(Schz_delu_dis_F25_onset_end_death_time, Schz_delu_dis_F25_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 +  Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_SchF25ref1) 
summary (cox_SchF25ref1_model)

####ref2
selected_data_SchF25ref2 <- data %>%
  select(Schz_delu_dis_F25_onset_end_death_time, Schz_delu_dis_F25_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_SchF25ref2_model <- coxph(Surv(Schz_delu_dis_F25_onset_end_death_time, Schz_delu_dis_F25_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 +  Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_SchF25ref2) 
summary (cox_SchF25ref2_model)

####ref3
selected_data_SchF25ref3 <- data %>%
  select(Schz_delu_dis_F25_onset_end_death_time, Schz_delu_dis_F25_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_SchF25ref3_model <- coxph(Surv(Schz_delu_dis_F25_onset_end_death_time, Schz_delu_dis_F25_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 +  Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_SchF25ref3) 
summary (cox_SchF25ref3_model)

####ref4
selected_data_SchF25ref4 <- data %>%
  select(Schz_delu_dis_F25_onset_end_death_time, Schz_delu_dis_F25_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03,Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_SchF25ref4_model <- coxph(Surv(Schz_delu_dis_F25_onset_end_death_time, Schz_delu_dis_F25_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 +  Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_SchF25ref4) 
summary (cox_SchF25ref4_model)



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
fitted_models_schizo <- list()

# Loop to fit models and store results
for (i in names(models)) {
  fitted_models_schizo [[i]] <- survreg(Surv(Schz_delu_dis_F20_F25_onset_end_death_time, Schz_delu_dis_F20_F25_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 +  Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_Sch, dist = models[[i]])
}

# View summaries and calculate AIC/BIC
aic_values_schizo <- sapply(fitted_models_schizo , AIC)
bic_values_schizo <- sapply(fitted_models_schizo , BIC)

# Print summaries
lapply(fitted_models_schizo , summary)

# Print AIC and BIC values
print(aic_values_schizo)
print(bic_values_schizo)


# Calculate and display time ratios
time_ratios_schizo <- exp(coef(fitted_models_schizo [["Loglogistic"]]))
print(time_ratios_schizo)

# Calculate and display time ratios for the Loglogistic model
# Extract coefficients and their standard errors
coef_loglogistic_schizo <- coef(fitted_models_schizo[["Loglogistic"]])
se_loglogistic_schizo <- sqrt(diag(vcov(fitted_models_schizo[["Loglogistic"]])))

# Calculate 95% confidence intervals on the log-time scale
ci_low_loglogistic_schizo <- coef_loglogistic_schizo - 1.96 * se_loglogistic_schizo
ci_high_loglogistic_schizo <- coef_loglogistic_schizo + 1.96 * se_loglogistic_schizo

# Transform confidence intervals to the time scale
time_ratio_lower_loglogistic_schizo <- exp(ci_low_loglogistic_schizo)
time_ratio_upper_loglogistic_schizo <- exp(ci_high_loglogistic_schizo)

# Calculate time ratios
time_ratios_loglogistic_schizo <- exp(coef_loglogistic_schizo)

# Print results for Loglogistic model
print("Time Ratios for Loglogistic Model:")
print(time_ratios_loglogistic_schizo)
print("95% Confidence Intervals Lower Bounds:")
print(time_ratio_lower_loglogistic_schizo)
print("95% Confidence Intervals Upper Bounds:")
print(time_ratio_upper_loglogistic_schizo)
