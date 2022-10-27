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
d[d == "Masculino"] <- "1"
d[d == "Femenino"] <- "0"
d$gender <- as.integer(d$gender)

# adding the meta_d column
meta_d <- read.table("./Analysis/Meta_d_analysis/meta_d.txt")

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

# model 2 - with out gender and age
a2=lm(meta_d ~ AQ_test.std,
     data = d) 
summary(a2)

save(a2, file = "Data/Regression_Results/meta_d_AQ_linear_model_2.RData")
