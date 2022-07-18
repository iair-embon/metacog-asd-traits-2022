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
df_total$gender <- ifelse(df_total$gender == "Masculino",1,0)
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

save(a_log, file = "Data/Regression_Results/MixedLogisticRegressionAnalysis.RData")