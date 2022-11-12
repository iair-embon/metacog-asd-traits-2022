##########################################
### Mixed Logistic Regression Analysis ###
##########################################

require(lme4)

root <- rprojroot::is_rstudio_project
basename(getwd())

####### data frame with filters already applied
filepath <- root$find_file("Data/df_total.Rda")
load(file= filepath)

# Filter by reaction times
df_total <- df_total[df_total$TimeDiscTrial <= 20000,]
df_total <- df_total[df_total$TimeConfTrial <=20000,]

# burning the first 20 trials of each subject
df_total <- df_total[df_total$trial > 20,]

# preprocessing
df_total[df_total == "non gender"] <- "4"
df_total[df_total == "Non Binary"] <- "3"
df_total[df_total == "Prefer not to say"] <- "2"
df_total[df_total == "Male"] <- "1"
df_total[df_total == "Female"] <- "0"
df_total$gender <- as.integer(df_total$gender)
df_total$discrimination_is_correct <- ifelse(df_total$discrimination_is_correct == TRUE, 1, 0)
df_total$Participant <- factor(df_total$Participant)
df_total$confidence_key.norm <- (df_total$confidence_key - 1) / 3 
df_total$AQ_test.std <-  (df_total$AQ_test - mean(df_total$AQ_test)) / sd(df_total$AQ_test)
df_total$age.std <- (df_total$age - mean(df_total$age))/sd(df_total$age)

a_log <- glmer(discrimination_is_correct ~ confidence_key.norm +
                 confidence_key.norm:AQ_test.std +
                 confidence_key.norm:gender +
                 confidence_key.norm:age.std +
                 confidence_key.norm:AQ_test.std:gender +
                 confidence_key.norm:AQ_test.std:age.std +
                 (1|Participant),
               data = df_total,
               family = binomial,
               control=glmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=2e5)))

summary(a_log)

save(a_log, file = "Data/Regression_Results/Non_Exclusion_Criteria/MixedLogisticRegressionAnalysis.RData")

# Adjust p-values for multiple comparisons with fdr method
library(broom)
library(tidyverse)

regression.results = summary(a_log)$coefficients
p_values <-unname(regression.results[,4])
p.adjust(p_values, method = "fdr")
