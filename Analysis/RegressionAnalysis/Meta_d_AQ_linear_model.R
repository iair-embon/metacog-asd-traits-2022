##########################################
### Linear Regression Analysis Meta d' ### 
##########################################

### linear regression model 
root <- rprojroot::is_rstudio_project
basename(getwd())               

####### data frame with filters already applied
filepath <- root$find_file("Data/df_total_filtered.Rda")
load(file= filepath)

source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_subset.R"))
d <- DataFrame_subset(df_total_filtered)

### preprocessing
d$AQ_test.std <- (d$AQ_test - mean(d$AQ_test))/ sd(d$AQ_test)
d$age.std <- (d$age - mean(d$age))/ sd(d$age)
d[d == "Male"] <- "1"
d[d == "Female"] <- "0"
d$gender <- as.integer(d$gender)

# adding the meta_d column
meta_d <- read.table("./Analysis/Meta_d_analysis/meta_d_filtered.txt")

d$meta_d <- meta_d$V1

# model 
a=lm(meta_d ~ AQ_test.std +
       gender +
       age.std +
       AQ_test.std: gender +
       AQ_test.std:age.std,
     data = d) 
summary(a)

save(a, file = "Data/Regression_Results/Meta_d_AQ_linear_model.RData")

# power analysis
library(pwr)

power <- pwr.f2.test(u = 3, # number of iv
                     v = a$df.residual, # degree of freedom
                     f2 = 0.002147/(1-0.002147), # effect size
                     sig.level = 0.05) # alpha

print(paste("power model 1:", power$power))

# Adjust p-values for multiple comparisons with fdr method
library(broom)
library(tidyverse)

regression.results = summary(a)$coefficients
p_values <-unname(regression.results[,4])
p.adjust(p_values, method = "fdr")

# model 2 - with out gender and age
a2=lm(meta_d ~ AQ_test.std,
     data = d) 
summary(a2)

save(a2, file = "Data/Regression_Results/meta_d_AQ_linear_model_2.RData")

# power analysis
power <- pwr.f2.test(u = 1, # number of iv
                     v = a2$df.residual, # degree of freedom
                     f2 = 0.0009014/(1-0.0009014), # effect size
                     sig.level = 0.05) # alpha

print(paste("power model 2:", power$power))

# Adjust p-values for multiple comparisons with fdr method
regression.results = summary(a2)$coefficients
p_values <-unname(regression.results[,4])
p.adjust(p_values, method = "fdr")
