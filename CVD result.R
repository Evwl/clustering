install.packages(c("survival", "survminer"))
library("survival")
library("survminer")
library("foreign")
library("tidyverse")
library("haven")
library("dplyr")

data<- read_sav ("/Users/evonwong/Library/CloudStorage/OneDrive-TheEducationUniversityofHongKong/Evon/UKBB/Clustering/[26 June CVD].sav")
# View the first few rows of the dataset
head(data)
# Summary of the dataset
summary(data)
# View the structure of the dataset
str(data)

############cvd
selected_data_cvd <- data %>%
  select(cvd_onset_end_death_time, cvd_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode)
  )
library(survival)
cox_cvd_model <- coxph(Surv(cvd_onset_end_death_time, cvd_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvd) 
summary (cox_cvd_model)


# Example of running separate models for each income group
income_levels <- levels(selected_data_cvd$Ave_Household_Income_mir_v2)
models_by_income <- lapply(income_levels, function(income) {
  data_subset <- filter(selected_data_cvd, Ave_Household_Income_mir_v2 == income)
  coxph(Surv(cvd_onset_end_death_time, cvd_onset_end_death_0_1) ~ 
          Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Cluster_k4_v2_recode, 
        data = data_subset)
})

# Example of summarizing one of the models
summary(models_by_income[[1]])  # Replace 1 with appropriate index for specific income level
# Example of summarizing one of the models
summary(models_by_income[[2]])  # Replace 1 with appropriate index for specific income level



############cvd ref2
selected_data_cvdref2 <- data %>%
  select(cvd_onset_end_death_time, cvd_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdref2_model <- coxph(Surv(cvd_onset_end_death_time, cvd_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdref2) 
summary (cox_cvdref2_model)
############cvd ref3
selected_data_cvdref3 <- data %>%
  select(cvd_onset_end_death_time, cvd_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdref3_model <- coxph(Surv(cvd_onset_end_death_time, cvd_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdref3) 
summary (cox_cvdref3_model)

############cvd ref4
selected_data_cvdref4 <- data %>%
  select(cvd_onset_end_death_time, cvd_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdref4_model <- coxph(Surv(cvd_onset_end_death_time, cvd_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdref4) 
summary (cox_cvdref4_model)

#################COXPHCVD##################
######I10-I15 ref1
selected_data_cvdI10_15ref1 <- data %>%
  select(cvd_I10_I15_onset_end_death_time, cvd_I10_I15_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI10_15ref1_model <- coxph(Surv(cvd_I10_I15_onset_end_death_time, cvd_I10_I15_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI10_15ref1) 
summary (cox_cvdI10_15ref1_model)

######I10-I15 ref2
selected_data_cvdI10_15ref2 <- data %>%
  select(cvd_I10_I15_onset_end_death_time, cvd_I10_I15_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI10_15ref2_model <- coxph(Surv(cvd_I10_I15_onset_end_death_time, cvd_I10_I15_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI10_15ref2) 
summary (cox_cvdI10_15ref2_model)

######I10-I15 ref3
selected_data_cvdI10_15ref3 <- data %>%
  select(cvd_I10_I15_onset_end_death_time, cvd_I10_I15_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI10_15ref3_model <- coxph(Surv(cvd_I10_I15_onset_end_death_time, cvd_I10_I15_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI10_15ref3) 
summary (cox_cvdI10_15ref3_model)

######I10-I15 ref4
selected_data_cvdI10_15ref4 <- data %>%
  select(cvd_I10_I15_onset_end_death_time, cvd_I10_I15_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI10_15ref4_model <- coxph(Surv(cvd_I10_I15_onset_end_death_time, cvd_I10_I15_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI10_15ref4) 
summary (cox_cvdI10_15ref4_model)

#######################
######I20-I25 ref1
selected_data_cvdI20_25ref1 <- data %>%
  select(cvd_I20_I25_onset_end_death_time, cvd_I20_I25_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),     
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI20_25ref1_model <- coxph(Surv(cvd_I20_I25_onset_end_death_time, cvd_I20_I25_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI20_25ref1) 
summary (cox_cvdI20_25ref1_model)

######I20-I25 ref2
selected_data_cvdI20_25ref2 <- data %>%
  select(cvd_I20_I25_onset_end_death_time, cvd_I20_I25_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI20_25ref2_model <- coxph(Surv(cvd_I20_I25_onset_end_death_time, cvd_I20_I25_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI20_25ref2) 
summary (cox_cvdI20_25ref2_model)

######I20-I25 ref3
selected_data_cvdI20_25ref3 <- data %>%
  select(cvd_I20_I25_onset_end_death_time, cvd_I20_I25_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI20_25ref3_model <- coxph(Surv(cvd_I20_I25_onset_end_death_time, cvd_I20_I25_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI20_25ref3) 
summary (cox_cvdI20_25ref3_model)

######I20-I25 ref4
selected_data_cvdI20_25ref4 <- data %>%
  select(cvd_I20_I25_onset_end_death_time, cvd_I20_I25_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI20_25ref4_model <- coxph(Surv(cvd_I20_I25_onset_end_death_time, cvd_I20_I25_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI20_25ref4) 
summary (cox_cvdI20_25ref4_model)

##################################I26-I28######
#####I26-28ref1
selected_data_cvdI26_28ref1 <- data %>%
  select(cvd_I26_I28_onset_end_death_time, cvd_I26_I28_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI26_28ref1_model <- coxph(Surv(cvd_I26_I28_onset_end_death_time, cvd_I26_I28_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI26_28ref1) 
summary (cox_cvdI26_28ref1_model)

#####I26-28ref2
selected_data_cvdI26_28ref2 <- data %>%
  select(cvd_I26_I28_onset_end_death_time, cvd_I26_I28_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI26_28ref2_model <- coxph(Surv(cvd_I26_I28_onset_end_death_time, cvd_I26_I28_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI26_28ref2) 
summary (cox_cvdI26_28ref2_model)

#####I26-28ref3
selected_data_cvdI26_28ref3 <- data %>%
  select(cvd_I26_I28_onset_end_death_time, cvd_I26_I28_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI26_28ref3_model <- coxph(Surv(cvd_I26_I28_onset_end_death_time, cvd_I26_I28_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI26_28ref3) 
summary (cox_cvdI26_28ref3_model)

#####I26-28ref4
selected_data_cvdI26_28ref4 <- data %>%
  select(cvd_I26_I28_onset_end_death_time, cvd_I26_I28_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),     
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI26_28ref4_model <- coxph(Surv(cvd_I26_I28_onset_end_death_time, cvd_I26_I28_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI26_28ref4) 
summary (cox_cvdI26_28ref4_model)

####################I30-I35###################
#####I30-52ref1
selected_data_cvdI30_52ref1 <- data %>%
  select(cvd_I30_I52_onset_end_death_time, cvd_I30_I52_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI30_52ref1_model <- coxph(Surv(cvd_I30_I52_onset_end_death_time, cvd_I30_I52_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI30_52ref1) 
summary (cox_cvdI30_52ref1_model)

#####I30-52ref2
selected_data_cvdI30_52ref2 <- data %>%
  select(cvd_I30_I52_onset_end_death_time, cvd_I30_I52_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI30_52ref2_model <- coxph(Surv(cvd_I30_I52_onset_end_death_time, cvd_I30_I52_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI30_52ref2) 
summary (cox_cvdI30_52ref2_model)

#####I30-52ref3
selected_data_cvdI30_52ref3 <- data %>%
  select(cvd_I30_I52_onset_end_death_time, cvd_I30_I52_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI30_52ref3_model <- coxph(Surv(cvd_I30_I52_onset_end_death_time, cvd_I30_I52_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI30_52ref3) 
summary (cox_cvdI30_52ref3_model)

#####I30-52ref4
selected_data_cvdI30_52ref4 <- data %>%
  select(cvd_I30_I52_onset_end_death_time, cvd_I30_I52_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI30_52ref4_model <- coxph(Surv(cvd_I30_I52_onset_end_death_time, cvd_I30_I52_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI30_52ref4) 
summary (cox_cvdI30_52ref4_model)

################################I60_I69
###I60-I69 ref1
selected_data_cvdI60_69ref1 <- data %>%
  select(cvd_I60_I69_onset_end_death_time, cvd_I60_I69_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),     
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI60_69ref1_model <- coxph(Surv(cvd_I60_I69_onset_end_death_time, cvd_I60_I69_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI60_69ref1) 
summary (cox_cvdI60_69ref1_model)

###I60-I69 ref2
selected_data_cvdI60_69ref2 <- data %>%
  select(cvd_I60_I69_onset_end_death_time, cvd_I60_I69_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI60_69ref2_model <- coxph(Surv(cvd_I60_I69_onset_end_death_time, cvd_I60_I69_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI60_69ref2) 
summary (cox_cvdI60_69ref2_model)

###I60-I69 ref3
selected_data_cvdI60_69ref3 <- data %>%
  select(cvd_I60_I69_onset_end_death_time, cvd_I60_I69_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),     
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI60_69ref3_model <- coxph(Surv(cvd_I60_I69_onset_end_death_time, cvd_I60_I69_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI60_69ref3) 
summary (cox_cvdI60_69ref3_model)

###I60-I69 ref4
selected_data_cvdI60_69ref4 <- data %>%
  select(cvd_I60_I69_onset_end_death_time, cvd_I60_I69_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI60_69ref4_model <- coxph(Surv(cvd_I60_I69_onset_end_death_time, cvd_I60_I69_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI60_69ref4) 
summary (cox_cvdI60_69ref4_model)


##############################I70-79
###I70-I79 ref1
selected_data_cvdI70_79ref1 <- data %>%
  select(cvd_I70_I79_onset_end_death_time, cvd_I70_I79_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI70_79ref1_model <- coxph(Surv(cvd_I70_I79_onset_end_death_time, cvd_I70_I79_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI70_79ref1) 
summary (cox_cvdI70_79ref1_model)

###I70-I79 ref2
selected_data_cvdI70_79ref2 <- data %>%
  select(cvd_I70_I79_onset_end_death_time, cvd_I70_I79_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),     
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI70_79ref2_model <- coxph(Surv(cvd_I70_I79_onset_end_death_time, cvd_I70_I79_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI70_79ref2) 
summary (cox_cvdI70_79ref2_model)

###I70-I79 ref3
selected_data_cvdI70_79ref3 <- data %>%
  select(cvd_I70_I79_onset_end_death_time, cvd_I70_I79_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),     
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI70_79ref3_model <- coxph(Surv(cvd_I70_I79_onset_end_death_time, cvd_I70_I79_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI70_79ref3) 
summary (cox_cvdI70_79ref3_model)

###I70-I79 ref4
selected_data_cvdI70_79ref4 <- data %>%
  select(cvd_I70_I79_onset_end_death_time, cvd_I70_I79_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),     
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI70_79ref4_model <- coxph(Surv(cvd_I70_I79_onset_end_death_time, cvd_I70_I79_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI70_79ref4) 
summary (cox_cvdI70_79ref4_model)

##############################################I80_89###############
####I80_89ref1
selected_data_cvdI80_89ref1 <- data %>%
  select(cvd_I80_I89_onset_end_death_time, cvd_I80_I89_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI80_89ref1_model <- coxph(Surv(cvd_I80_I89_onset_end_death_time, cvd_I80_I89_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI80_89ref1) 
summary (cox_cvdI80_89ref1_model)

####I80_89ref2
selected_data_cvdI80_89ref2 <- data %>%
  select(cvd_I80_I89_onset_end_death_time, cvd_I80_I89_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI80_89ref2_model <- coxph(Surv(cvd_I80_I89_onset_end_death_time, cvd_I80_I89_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI80_89ref2) 
summary (cox_cvdI80_89ref2_model)

####I80_89ref3
selected_data_cvdI80_89ref3 <- data %>%
  select(cvd_I80_I89_onset_end_death_time, cvd_I80_I89_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI80_89ref3_model <- coxph(Surv(cvd_I80_I89_onset_end_death_time, cvd_I80_I89_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI80_89ref3) 
summary (cox_cvdI80_89ref3_model)

####I80_89ref4
selected_data_cvdI80_89ref4 <- data %>%
  select(cvd_I80_I89_onset_end_death_time, cvd_I80_I89_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI80_89ref4_model <- coxph(Surv(cvd_I80_I89_onset_end_death_time, cvd_I80_I89_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI80_89ref4) 
summary (cox_cvdI80_89ref4_model)

###################I95-I99##########
###I95-I99 ref1
selected_data_cvdI95_99ref1 <- data %>%
  select(cvd_I95_I99_onset_end_death_time, cvd_I95_I99_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI95_99ref1_model <- coxph(Surv(cvd_I95_I99_onset_end_death_time, cvd_I95_I99_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI95_99ref1) 
summary (cox_cvdI95_99ref1_model)

###I95-I99 ref2
selected_data_cvdI95_99ref2 <- data %>%
  select(cvd_I95_I99_onset_end_death_time, cvd_I95_I99_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),     
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI95_99ref2_model <- coxph(Surv(cvd_I95_I99_onset_end_death_time, cvd_I95_I99_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI95_99ref2) 
summary (cox_cvdI95_99ref2_model)

###I95-I99 ref3
selected_data_cvdI95_99ref3 <- data %>%
  select(cvd_I95_I99_onset_end_death_time, cvd_I95_I99_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),     
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI95_99ref3_model <- coxph(Surv(cvd_I95_I99_onset_end_death_time, cvd_I95_I99_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI95_99ref3) 
summary (cox_cvdI95_99ref3_model)


###I95-I99 ref4
selected_data_cvdI95_99ref4 <- data %>%
  select(cvd_I95_I99_onset_end_death_time, cvd_I95_I99_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03),      
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2), 
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_cvdI95_99ref4_model <- coxph(Surv(cvd_I95_I99_onset_end_death_time, cvd_I95_I99_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvdI95_99ref4) 
summary (cox_cvdI95_99ref4_model)
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
fitted_models_CVD <- list()

# Loop to fit models and store results
for (i in names(models)) {
  fitted_models_CVD [[i]] <- survreg(Surv(cvd_onset_end_death_time, cvd_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cvd, dist = models[[i]])
}

# View summaries and calculate AIC/BIC
aic_values_CVD <- sapply(fitted_models_CVD , AIC)
bic_values_CVD <- sapply(fitted_models_CVD , BIC)

# Print summaries
lapply(fitted_models_CVD , summary)

# Print AIC and BIC values
print(aic_values_CVD)
print(bic_values_CVD)


# Calculate and display time ratios
time_ratios_CVD <- exp(coef(fitted_models_CVD [["Weibull"]]))
print(time_ratios_CVD)

# Calculate and display time ratios for the Weibull model
# Extract coefficients and their standard errors
coef_weibull_CVD <- coef(fitted_models_CVD[["Weibull"]])
se_weibull_CVD <- sqrt(diag(vcov(fitted_models_CVD[["Weibull"]])))

# Calculate 95% confidence intervals on the log-time scale
ci_low_weibull_CVD <- coef_weibull_CVD - 1.96 * se_weibull_CVD
ci_high_weibull_CVD <- coef_weibull_CVD + 1.96 * se_weibull_CVD

# Transform confidence intervals to the time scale
time_ratio_lower_weibull_CVD <- exp(ci_low_weibull_CVD)
time_ratio_upper_weibull_CVD <- exp(ci_high_weibull_CVD)

# Calculate time ratios
time_ratios_weibull_CVD <- exp(coef_weibull_CVD)

# Print results for Weibull model
print("Time Ratios for Weibull Model:")
print(time_ratios_weibull_CVD)
print("95% Confidence Intervals Lower Bounds:")
print(time_ratio_lower_weibull_CVD)
print("95% Confidence Intervals Upper Bounds:")
print(time_ratio_upper_weibull_CVD)

