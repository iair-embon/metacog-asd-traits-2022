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
d$AQ_test.norm <- (d$AQ_test - mean(d$AQ_test))/ sd(d$AQ_test)
d$mc.norm <- (d$mc - mean(d$mc))/ sd(d$mc)
d$age.norm <- (d$age - mean(d$age))/ sd(d$age)
d$m_c.norm <- (d$m_c - mean(d$m_c))/ sd(d$m_c)
d[d == "Masculino"] <- "1"
d[d == "Femenino"] <- "0"
d$gender <- as.integer(d$gender)

# model
a=lm(m_c.norm ~ AQ_test.norm +
       gender +
       age.norm +
       AQ_test.norm: gender +
       AQ_test.norm:age.norm,
     data = d) 
summary(a)

save(a, file = "Data/Regression_Results/ConfidenceMean_AQ_linear_model.RData")
