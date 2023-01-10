##########################################
### Mixed Logistic Regression Analysis ###
##########################################

require(lme4)

root <- rprojroot::is_rstudio_project
basename(getwd())

####### data frame with filters already applied
filepath <- root$find_file("Data/df_total_filtered.Rda")
load(file= filepath)

# preprocessing
df_total_filtered$gender <- ifelse(df_total_filtered$gender == "Male",1,0)
df_total_filtered$discrimination_is_correct <- ifelse(df_total_filtered$discrimination_is_correct == TRUE, 1, 0)
df_total_filtered$Participant <- factor(df_total_filtered$Participant)
df_total_filtered$confidence_key.norm <- (df_total_filtered$confidence_key - 1) / 3 
df_total_filtered$AQ_test.std <-  (df_total_filtered$AQ_test - mean(df_total_filtered$AQ_test)) / sd(df_total_filtered$AQ_test)
df_total_filtered$age.std <- (df_total_filtered$age - mean(df_total_filtered$age))/sd(df_total_filtered$age)

a_log <- glmer(discrimination_is_correct ~ confidence_key.norm +
                 confidence_key.norm:AQ_test.std +
                 confidence_key.norm:gender +
                 confidence_key.norm:age.std +
                 confidence_key.norm:AQ_test.std:gender +
                 confidence_key.norm:AQ_test.std:age.std +
                 (1|Participant),
               data = df_total_filtered,
               family = binomial,
               control=glmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=2e5)))

summary(a_log)

save(a_log, file = "Data/Regression_Results/MixedLogisticRegressionAnalysis.RData")

# power analysis
library(simr)

p1 <-powerSim(a_log, test = fixed("confidence_key.norm:AQ_test.std") ,nsim=5000)

print(p1)

save(p1, file = "Data/Regression_Results/Power1_MixedLogisticRegressionAnalysis.RData")

# Adjust p-values for multiple comparisons with fdr method
library(broom)
library(tidyverse)

regression.results = summary(a_log)$coefficients
p_values <-unname(regression.results[,4])
p.adjust(p_values, method = "fdr")

# model 2 - with out gender and age
a_log2 <- glmer(discrimination_is_correct ~ confidence_key.norm +
                 confidence_key.norm:AQ_test.std +
                 (1|Participant),
               data = df_total_filtered,
               family = binomial,
               control=glmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=2e5)))
summary(a_log2)

save(a_log2, file = "Data/Regression_Results/MixedLogisticRegressionAnalysis_2.RData")

# Adjust p-values for multiple comparisons with fdr method
regression.results = summary(a_log2)$coefficients
p_values <-unname(regression.results[,4])
p.adjust(p_values, method = "fdr")
