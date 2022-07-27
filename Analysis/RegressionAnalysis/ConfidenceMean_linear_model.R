########################################## 
### Regression model - confidence mean ### 
##########################################

### linear regression model 
root <- rprojroot::is_rstudio_project
basename(getwd())               

####### data frame with filters already applied
filepath <- root$find_file("Data/df_total_filtered.Rda")
load(file= filepath)

source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_subset.R"))
d <- DataFrame_subset(df_total)

### preprocessing
d$AQ_test.std <- (d$AQ_test - mean(d$AQ_test))/ sd(d$AQ_test)
d$age.std <- (d$age - mean(d$age))/ sd(d$age)
d$ConfMean.std <- (d$ConfMean - mean(d$ConfMean))/ sd(d$ConfMean)
d[d == "Masculino"] <- "1"
d[d == "Femenino"] <- "0"
d$gender <- as.integer(d$gender)

# model
a=lm(ConfMean.std ~ AQ_test.std +
       gender +
       age.std +
       AQ_test.std: gender +
       AQ_test.std:age.std,
     data = d) 
summary(a)

save(a, file = "Data/Regression_Results/ConfidenceMean_AQ_linear_model.RData")
