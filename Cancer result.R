library("survival")
library("survminer")
library("foreign")
library("tidyverse")
library("haven")
library("dplyr")

data<- read_sav ("/Users/evonwong/Library/CloudStorage/OneDrive-TheEducationUniversityofHongKong/Evon/UKBB/Clustering/[26 June cancer].sav")

head(data)
# Summary of the dataset
summary(data)
# View the structure of the dataset
str(data)

################cancer ref1##########
selected_data_cancer <- data %>%
  select(Malignant_Cancer_C00_C97_onset_end_death_time, Malignant_Cancer_C00_C97_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode)
  )
library(survival)
cox_cancer_model <- coxph(Surv(Malignant_Cancer_C00_C97_onset_end_death_time, Malignant_Cancer_C00_C97_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cancer) 
summary (cox_cancer_model)

# Example of running separate models for each income group
income_levels <- levels(selected_data_cancer$Ave_Household_Income_mir_v2)
models_by_income <- lapply(income_levels, function(income) {
  data_subset <- filter(selected_data_cancer, Ave_Household_Income_mir_v2 == income)
  coxph(Surv(Malignant_Cancer_C00_C97_onset_end_death_time, Malignant_Cancer_C00_C97_onset_end_death_0_1) ~ 
          Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Cluster_k4_v2_recode, 
        data = data_subset)
})

# Example of summarizing one of the models
summary(models_by_income[[1]])  # Replace 1 with appropriate index for specific income level
# Example of summarizing one of the models
summary(models_by_income[[2]])  # Replace 1 with appropriate index for specific income level



################cancer ref2##########
selected_data_cancerref2 <- data %>%
  select(Malignant_Cancer_C00_C97_onset_end_death_time, Malignant_Cancer_C00_C97_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_cancerref2_model <- coxph(Surv(Malignant_Cancer_C00_C97_onset_end_death_time, Malignant_Cancer_C00_C97_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cancerref2) 
summary (cox_cancerref2_model)

################cancer ref3##########
selected_data_cancerref3 <- data %>%
  select(Malignant_Cancer_C00_C97_onset_end_death_time, Malignant_Cancer_C00_C97_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_cancerref3_model <- coxph(Surv(Malignant_Cancer_C00_C97_onset_end_death_time, Malignant_Cancer_C00_C97_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cancerref3) 
summary (cox_cancerref3_model)
################cancer ref4##########
selected_data_cancerref4 <- data %>%
  select(Malignant_Cancer_C00_C97_onset_end_death_time, Malignant_Cancer_C00_C97_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_cancerref4_model <- coxph(Surv(Malignant_Cancer_C00_C97_onset_end_death_time, Malignant_Cancer_C00_C97_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cancerref4) 
summary (cox_cancerref4_model)


################C50 breast cancer###########
#######ref(1)
selected_data_breastcancerref1 <- data %>%
  select(C50_M_cancer_breast_onset_end_death_time, C50_M_cancer_breast_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_breastcancerref1_model <- coxph(Surv(C50_M_cancer_breast_onset_end_death_time, C50_M_cancer_breast_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_breastcancerref1) 
summary (cox_breastcancerref1_model)
#########ref2)
selected_data_breastcancerref2 <- data %>%
  select(C50_M_cancer_breast_onset_end_death_time, C50_M_cancer_breast_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_breastcancerref2_model <- coxph(Surv(C50_M_cancer_breast_onset_end_death_time, C50_M_cancer_breast_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_breastcancerref2) 
summary (cox_breastcancerref2_model)

#########ref3)
selected_data_breastcancerref3 <- data %>%
  select(C50_M_cancer_breast_onset_end_death_time, C50_M_cancer_breast_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_breastcancerref3_model <- coxph(Surv(C50_M_cancer_breast_onset_end_death_time, C50_M_cancer_breast_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_breastcancerref3) 
summary (cox_breastcancerref3_model)

#########ref4)
selected_data_breastcancerref4 <- data %>%
  select(C50_M_cancer_breast_onset_end_death_time, C50_M_cancer_breast_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_breastcancerref4_model <- coxph(Surv(C50_M_cancer_breast_onset_end_death_time, C50_M_cancer_breast_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_breastcancerref4) 
summary (cox_breastcancerref4_model)

#################################prostate cancer #########################3
####ref(1)
selected_data_prostatecancerref1 <- data %>%
  select(C61_Prostate_cancer_onset_end_death_time, C61_Prostate_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_prostatecancerref1_model <- coxph(Surv(C61_Prostate_cancer_onset_end_death_time, C61_Prostate_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_prostatecancerref1) 
summary (cox_prostatecancerref1_model)

####ref(2)
selected_data_prostatecancerref2 <- data %>%
  select(C61_Prostate_cancer_onset_end_death_time, C61_Prostate_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_prostatecancerref2_model <- coxph(Surv(C61_Prostate_cancer_onset_end_death_time, C61_Prostate_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_prostatecancerref2) 
summary (cox_prostatecancerref2_model)

####ref(3)
selected_data_prostatecancerref3 <- data %>%
  select(C61_Prostate_cancer_onset_end_death_time, C61_Prostate_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_prostatecancerref3_model <- coxph(Surv(C61_Prostate_cancer_onset_end_death_time, C61_Prostate_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_prostatecancerref3) 
summary (cox_prostatecancerref3_model)

####ref(4)
selected_data_prostatecancerref4 <- data %>%
  select(C61_Prostate_cancer_onset_end_death_time, C61_Prostate_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_prostatecancerref4_model <- coxph(Surv(C61_Prostate_cancer_onset_end_death_time, C61_Prostate_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_prostatecancerref4) 
summary (cox_prostatecancerref4_model)

###################################C18 colorectal cancer######################
####ref(1)
selected_data_colorectalcancerref1 <- data %>%
  select(C18_colorectal_cancer_onset_end_death_time, C18_colorectal_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_colorectalcancerref1_model <- coxph(Surv(C18_colorectal_cancer_onset_end_death_time, C18_colorectal_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_colorectalcancerref1) 
summary (cox_colorectalcancerref1_model)
####ref(2)
selected_data_colorectalcancerref2 <- data %>%
  select(C18_colorectal_cancer_onset_end_death_time, C18_colorectal_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_colorectalcancerref2_model <- coxph(Surv(C18_colorectal_cancer_onset_end_death_time, C18_colorectal_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_colorectalcancerref2) 
summary (cox_colorectalcancerref2_model)

####ref(3)
selected_data_colorectalcancerref3 <- data %>%
  select(C18_colorectal_cancer_onset_end_death_time, C18_colorectal_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_colorectalcancerref3_model <- coxph(Surv(C18_colorectal_cancer_onset_end_death_time, C18_colorectal_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_colorectalcancerref3) 
summary (cox_colorectalcancerref3_model)

####ref(4)
selected_data_colorectalcancerref4 <- data %>%
  select(C18_colorectal_cancer_onset_end_death_time, C18_colorectal_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_colorectalcancerref4_model <- coxph(Surv(C18_colorectal_cancer_onset_end_death_time, C18_colorectal_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_colorectalcancerref4) 
summary (cox_colorectalcancerref4_model)


#########################C43 skin cancer#################################3
#####ref1#
selected_data_skincancerref1 <- data %>%
  select(C43_skin_melanoma_cancer_onset_end_death_time, C43_skin_melanoma_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_skincancerref1_model <- coxph(Surv(C43_skin_melanoma_cancer_onset_end_death_time, C43_skin_melanoma_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_skincancerref1) 
summary (cox_skincancerref1_model)
########ref2#####
selected_data_skincancerref2 <- data %>%
  select(C43_skin_melanoma_cancer_onset_end_death_time, C43_skin_melanoma_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_skincancerref2_model <- coxph(Surv(C43_skin_melanoma_cancer_onset_end_death_time, C43_skin_melanoma_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_skincancerref2) 
summary (cox_skincancerref2_model)
########ref3#####
selected_data_skincancerref3 <- data %>%
  select(C43_skin_melanoma_cancer_onset_end_death_time, C43_skin_melanoma_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_skincancerref3_model <- coxph(Surv(C43_skin_melanoma_cancer_onset_end_death_time, C43_skin_melanoma_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_skincancerref3) 
summary (cox_skincancerref3_model)

########ref4#####
selected_data_skincancerref4 <- data %>%
  select(C43_skin_melanoma_cancer_onset_end_death_time, C43_skin_melanoma_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_skincancerref4_model <- coxph(Surv(C43_skin_melanoma_cancer_onset_end_death_time, C43_skin_melanoma_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_skincancerref4) 
summary (cox_skincancerref4_model)


###########lymphoma cancer######
#####ref1#
selected_data_lymphomacancerref1 <- data %>%
  select(C81_C88_lymphoma_cancer_onset_end_death_time, C81_C88_lymphoma_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_lymphomacancerref1_model <- coxph(Surv(C81_C88_lymphoma_cancer_onset_end_death_time, C81_C88_lymphoma_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_lymphomacancerref1) 
summary (cox_lymphomacancerref1_model)

#####ref2#
selected_data_lymphomacancerref2 <- data %>%
  select(C81_C88_lymphoma_cancer_onset_end_death_time, C81_C88_lymphoma_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_lymphomacancerref2_model <- coxph(Surv(C81_C88_lymphoma_cancer_onset_end_death_time, C81_C88_lymphoma_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_lymphomacancerref2) 
summary (cox_lymphomacancerref2_model)

#####ref3#
selected_data_lymphomacancerref3 <- data %>%
  select(C81_C88_lymphoma_cancer_onset_end_death_time, C81_C88_lymphoma_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_lymphomacancerref3_model <- coxph(Surv(C81_C88_lymphoma_cancer_onset_end_death_time, C81_C88_lymphoma_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_lymphomacancerref3) 
summary (cox_lymphomacancerref3_model)

#####ref4#
selected_data_lymphomacancerref4 <- data %>%
  select(C81_C88_lymphoma_cancer_onset_end_death_time, C81_C88_lymphoma_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_lymphomacancerref4_model <- coxph(Surv(C81_C88_lymphoma_cancer_onset_end_death_time, C81_C88_lymphoma_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_lymphomacancerref4) 
summary (cox_lymphomacancerref4_model)

#####################################uterine cancer##############

#####ref1#
selected_data_uterinecancerref1 <- data %>%
  select(C66_Uterine_cancer_onset_end_death_time, C66_Uterine_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_uterinecancerref1_model <- coxph(Surv(C66_Uterine_cancer_onset_end_death_time, C66_Uterine_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_uterinecancerref1) 
summary (cox_uterinecancerref1_model)

#####ref2#
selected_data_uterinecancerref2 <- data %>%
  select(C66_Uterine_cancer_onset_end_death_time, C66_Uterine_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_uterinecancerref2_model <- coxph(Surv(C66_Uterine_cancer_onset_end_death_time, C66_Uterine_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_uterinecancerref2) 
summary (cox_uterinecancerref2_model)

#####ref3#
selected_data_uterinecancerref3 <- data %>%
  select(C66_Uterine_cancer_onset_end_death_time, C66_Uterine_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_uterinecancerref3_model <- coxph(Surv(C66_Uterine_cancer_onset_end_death_time, C66_Uterine_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_uterinecancerref3) 
summary (cox_uterinecancerref3_model)

#####ref4#
selected_data_uterinecancerref4 <- data %>%
  select(C66_Uterine_cancer_onset_end_death_time, C66_Uterine_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_uterinecancerref4_model <- coxph(Surv(C66_Uterine_cancer_onset_end_death_time, C66_Uterine_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_uterinecancerref4) 
summary (cox_uterinecancerref4_model)

##########################kidney cancer################
#####ref1#
selected_data_kidneycancerref1 <- data %>%
  select(C64_Kidney_cancer_onset_end_death_time, C64_Kidney_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_kidneycancerref1_model <- coxph(Surv(C64_Kidney_cancer_onset_end_death_time, C64_Kidney_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_kidneycancerref1) 
summary (cox_kidneycancerref1_model)


#####ref2#
selected_data_kidneycancerref2 <- data %>%
  select(C64_Kidney_cancer_onset_end_death_time, C64_Kidney_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_kidneycancerref2_model <- coxph(Surv(C64_Kidney_cancer_onset_end_death_time, C64_Kidney_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_kidneycancerref2) 
summary (cox_kidneycancerref2_model)

#####ref3#
selected_data_kidneycancerref3 <- data %>%
  select(C64_Kidney_cancer_onset_end_death_time, C64_Kidney_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_kidneycancerref3_model <- coxph(Surv(C64_Kidney_cancer_onset_end_death_time, C64_Kidney_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_kidneycancerref3) 
summary (cox_kidneycancerref3_model)

#####ref4#
selected_data_kidneycancerref4 <- data %>%
  select(C64_Kidney_cancer_onset_end_death_time, C64_Kidney_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_kidneycancerref4_model <- coxph(Surv(C64_Kidney_cancer_onset_end_death_time, C64_Kidney_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_kidneycancerref4) 
summary (cox_kidneycancerref4_model)

#################################pancreatic cancer##################
#####ref1#
selected_data_pancreaticcancerref1 <- data %>%
  select(C25_pancreatic_cancer_onset_end_death_time, C25_pancreatic_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_pancreaticcancerref1_model <- coxph(Surv(C25_pancreatic_cancer_onset_end_death_time, C25_pancreatic_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_pancreaticcancerref1) 
summary (cox_pancreaticcancerref1_model)

#####ref2#
selected_data_pancreaticcancerref2 <- data %>%
  select(C25_pancreatic_cancer_onset_end_death_time, C25_pancreatic_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_pancreaticcancerref2_model <- coxph(Surv(C25_pancreatic_cancer_onset_end_death_time, C25_pancreatic_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_pancreaticcancerref2) 
summary (cox_pancreaticcancerref2_model)


#####ref3#
selected_data_pancreaticcancerref3 <- data %>%
  select(C25_pancreatic_cancer_onset_end_death_time, C25_pancreatic_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_pancreaticcancerref3_model <- coxph(Surv(C25_pancreatic_cancer_onset_end_death_time, C25_pancreatic_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_pancreaticcancerref3) 
summary (cox_pancreaticcancerref3_model)

#####ref4#
selected_data_pancreaticcancerref4 <- data %>%
  select(C25_pancreatic_cancer_onset_end_death_time, C25_pancreatic_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_pancreaticcancerref4_model <- coxph(Surv(C25_pancreatic_cancer_onset_end_death_time, C25_pancreatic_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_pancreaticcancerref4) 
summary (cox_pancreaticcancerref4_model)
###########################Leukaemia cancer###############
#####ref1#
selected_data_Leukaemiacancerref1 <- data %>%
  select(C91_95_leukaemia_onset_end_death_time, C91_95_leukaemia_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_Leukaemiacancerref1_model <- coxph(Surv(C91_95_leukaemia_onset_end_death_time, C91_95_leukaemia_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_Leukaemiacancerref1) 
summary (cox_Leukaemiacancerref1_model)

#####ref2#
selected_data_Leukaemiacancerref2 <- data %>%
  select(C91_95_leukaemia_onset_end_death_time, C91_95_leukaemia_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_Leukaemiacancerref2_model <- coxph(Surv(C91_95_leukaemia_onset_end_death_time, C91_95_leukaemia_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_Leukaemiacancerref2) 
summary (cox_Leukaemiacancerref2_model)

#####ref3#
selected_data_Leukaemiacancerref3 <- data %>%
  select(C91_95_leukaemia_onset_end_death_time, C91_95_leukaemia_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_Leukaemiacancerref3_model <- coxph(Surv(C91_95_leukaemia_onset_end_death_time, C91_95_leukaemia_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_Leukaemiacancerref3) 
summary (cox_Leukaemiacancerref3_model)

#####ref4#
selected_data_Leukaemiacancerref4 <- data %>%
  select(C91_95_leukaemia_onset_end_death_time, C91_95_leukaemia_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_Leukaemiacancerref4_model <- coxph(Surv(C91_95_leukaemia_onset_end_death_time, C91_95_leukaemia_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_Leukaemiacancerref4) 
summary (cox_Leukaemiacancerref4_model)

######################bladder cancer #####################
########ref1
selected_data_bladdercancerref1 <- data %>%
  select(C67_Bladder_cancer_onset_end_death_time, C67_Bladder_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 1")  # Relevel to make the second category the reference
  )
library(survival)
cox_bladdercancerref1_model <- coxph(Surv(C67_Bladder_cancer_onset_end_death_time, C67_Bladder_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_bladdercancerref1) 
summary (cox_bladdercancerref1_model)

########ref2
selected_data_bladdercancerref2 <- data %>%
  select(C67_Bladder_cancer_onset_end_death_time, C67_Bladder_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 2")  # Relevel to make the second category the reference
  )
library(survival)
cox_bladdercancerref2_model <- coxph(Surv(C67_Bladder_cancer_onset_end_death_time, C67_Bladder_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_bladdercancerref2) 
summary (cox_bladdercancerref2_model)

########ref3
selected_data_bladdercancerref3 <- data %>%
  select(C67_Bladder_cancer_onset_end_death_time, C67_Bladder_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 3")  # Relevel to make the second category the reference
  )
library(survival)
cox_bladdercancerref3_model <- coxph(Surv(C67_Bladder_cancer_onset_end_death_time, C67_Bladder_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_bladdercancerref3) 
summary (cox_bladdercancerref3_model)

########ref4
selected_data_bladdercancerref4 <- data %>%
  select(C67_Bladder_cancer_onset_end_death_time, C67_Bladder_cancer_onset_end_death_0_1, Sex_mir, Age_Recruit, Ethnic_mir_gp03, Ave_Household_Income_mir_v2, Cluster_k4_v2_recode) %>%
  mutate(
    Sex_mir =  as_factor(Sex_mir),
    Ethnic_mir_gp03 = as_factor(Ethnic_mir_gp03), 
    Ave_Household_Income_mir_v2 = as_factor (Ave_Household_Income_mir_v2),
    Cluster_k4_v2_recode = as_factor(Cluster_k4_v2_recode) %>% 
      fct_relevel("Pattern 4")  # Relevel to make the second category the reference
  )
library(survival)
cox_bladdercancerref4_model <- coxph(Surv(C67_Bladder_cancer_onset_end_death_time, C67_Bladder_cancer_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_bladdercancerref4) 
summary (cox_bladdercancerref4_model)


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
fitted_models_Cancer <- list()

# Loop to fit models and store results
for (i in names(models)) {
  fitted_models_Cancer [[i]] <- survreg(Surv(Malignant_Cancer_C00_C97_onset_end_death_time, Malignant_Cancer_C00_C97_onset_end_death_0_1) ~ Sex_mir + Age_Recruit + Ethnic_mir_gp03 + Ave_Household_Income_mir_v2 + Cluster_k4_v2_recode, data = selected_data_cancer, dist = models[[i]])
}

# View summaries and calculate AIC/BIC
aic_values_Cancer <- sapply(fitted_models_Cancer , AIC)
bic_values_Cancer <- sapply(fitted_models_Cancer , BIC)

# Print summaries
lapply(fitted_models_Cancer , summary)

# Print AIC and BIC values
print(aic_values_Cancer)
print(bic_values_Cancer)


# Calculate and display time ratios
time_ratios_Cancer <- exp(coef(fitted_models_Cancer [["Loglogistic"]]))
print(time_ratios_Cancer)


# Calculate and display time ratios for the Loglogistic model
# Extract coefficients and their standard errors
coef_loglogistic_Cancer <- coef(fitted_models_Cancer[["Loglogistic"]])
se_loglogistic_Cancer <- sqrt(diag(vcov(fitted_models_Cancer[["Loglogistic"]])))

# Calculate 95% confidence intervals on the log-time scale
ci_low_loglogistic_Cancer <- coef_loglogistic_Cancer - 1.96 * se_loglogistic_Cancer
ci_high_loglogistic_Cancer <- coef_loglogistic_Cancer + 1.96 * se_loglogistic_Cancer

# Transform confidence intervals to the time scale
time_ratio_lower_loglogistic_Cancer <- exp(ci_low_loglogistic_Cancer)
time_ratio_upper_loglogistic_Cancer <- exp(ci_high_loglogistic_Cancer)

# Calculate time ratios
time_ratios_loglogistic_Cancer <- exp(coef_loglogistic_Cancer)

# Print results for Loglogistic model
print("Time Ratios for Loglogistic Model:")
print(time_ratios_loglogistic_Cancer)
print("95% Confidence Intervals Lower Bounds:")
print(time_ratio_lower_loglogistic_Cancer)
print("95% Confidence Intervals Upper Bounds:")
print(time_ratio_upper_loglogistic_Cancer)
