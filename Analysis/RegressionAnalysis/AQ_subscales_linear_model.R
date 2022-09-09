###############################################
### Linear Regression Analysis AQ Subscales ### 
###############################################

### linear regression model 

root <- rprojroot::is_rstudio_project
basename(getwd())               

####### data frame with filters already applied
filepath <- root$find_file("Data/df_total_filtered.Rda")
load(file= filepath)

source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_subset.R"))
d <- DataFrame_subset(df_total)

### preprocessing
d$age.std <- (d$age - mean(d$age))/ sd(d$age)
d[d == "Masculino"] <- "1"
d[d == "Femenino"] <- "0"
d$gender <- as.integer(d$gender)
d$AQ_social.std <- (d$AQ_social - mean(d$AQ_social))/ sd(d$AQ_social)
d$AQ_AttentionSwitch.std <- (d$AQ_AttentionSwitch - mean(d$AQ_AttentionSwitch))/ sd(d$AQ_AttentionSwitch)
d$AQ_AttentionDetail.std <- (d$AQ_AttentionDetail - mean(d$AQ_AttentionDetail))/ sd(d$AQ_AttentionDetail)
d$AQ_communication.std <- (d$AQ_communication - mean(d$AQ_communication))/ sd(d$AQ_communication)
d$AQ_imagination.std <- (d$AQ_imagination - mean(d$AQ_imagination))/ sd(d$AQ_imagination)

# model
a=lm(mc ~ AQ_social.std+
       AQ_AttentionSwitch.std+
       AQ_AttentionDetail.std+
       AQ_communication.std+
       AQ_imagination.std+
       gender +
       age.std,
     data = d) 
summary(a)

save(a, file = "Data/Regression_Results/AQ_subscales_linear_model.RData")

# model 2 - with out gender and age
a2=lm(mc ~ AQ_social.std+
       AQ_AttentionSwitch.std+
       AQ_AttentionDetail.std+
       AQ_communication.std+
       AQ_imagination.std,
     data = d) 
summary(a2)

save(a2, file = "Data/Regression_Results/AQ_subscales_linear_model_2.RData")
