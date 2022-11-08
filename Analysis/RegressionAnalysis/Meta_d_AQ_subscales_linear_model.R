######################################################
### Linear Regression Analysis AQ Subscales Meta_d ### 
######################################################

### linear regression model 

root <- rprojroot::is_rstudio_project
basename(getwd())               

####### data frame with filters already applied
filepath <- root$find_file("Data/df_total_filtered.Rda")
load(file= filepath)

source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_subset.R"))
d <- DataFrame_subset(df_total_filtered)

### preprocessing
d$age.std <- (d$age - mean(d$age))/ sd(d$age)
d[d == "Male"] <- "1"
d[d == "Female"] <- "0"
d$gender <- as.integer(d$gender)
d$AQ_social.std <- (d$AQ_social - mean(d$AQ_social))/ sd(d$AQ_social)
d$AQ_AttentionSwitch.std <- (d$AQ_AttentionSwitch - mean(d$AQ_AttentionSwitch))/ sd(d$AQ_AttentionSwitch)
d$AQ_AttentionDetail.std <- (d$AQ_AttentionDetail - mean(d$AQ_AttentionDetail))/ sd(d$AQ_AttentionDetail)
d$AQ_communication.std <- (d$AQ_communication - mean(d$AQ_communication))/ sd(d$AQ_communication)
d$AQ_imagination.std <- (d$AQ_imagination - mean(d$AQ_imagination))/ sd(d$AQ_imagination)

# adding the meta_d column
meta_d <- read.table("./Analysis/Meta_d_analysis/meta_d_filtered.txt")

d$meta_d <- meta_d$V1

# model
a=lm(meta_d ~ AQ_social.std+
       AQ_AttentionSwitch.std+
       AQ_AttentionDetail.std+
       AQ_communication.std+
       AQ_imagination.std+
       gender +
       age.std,
     data = d) 
summary(a)

save(a, file = "Data/Regression_Results/Meta_d_AQ_subscales_linear_model.RData")

# power analysis
library(pwr)

power <- pwr.f2.test(u = 7, # number of iv
                     v = a$df.residual, # degree of freedom
                     f2 = 0.01748/(1-0.01748), # effect size
                     sig.level = 0.05) # alpha

print(paste("power model 1:", power$power))

# model 2 - with out gender and age
a2=lm(meta_d ~ AQ_social.std+
       AQ_AttentionSwitch.std+
       AQ_AttentionDetail.std+
       AQ_communication.std+
       AQ_imagination.std,
     data = d) 
summary(a2)

save(a2, file = "Data/Regression_Results/Meta_d_AQ_subscales_linear_model_2.RData")

# power analysis
power <- pwr.f2.test(u = 5, # number of iv
                     v = a2$df.residual, # degree of freedom
                     f2 = 0.003178 /(1-0.003178), # effect size
                     sig.level = 0.05) # alpha

print(paste("power model 2:", power$power))
