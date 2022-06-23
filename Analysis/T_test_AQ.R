####################################
### T_test AQ by gender Analysis ###
####################################

root <- rprojroot::is_rstudio_project
basename(getwd())

####### data frames with filters already applied
filepath <- root$find_file("Data/df_total_filtered.Rda")
load(file= filepath)

source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_subset.R"))
d <- DataFrame_subset(df_total)

t.test(d[d$gender == "Femenino", "AQ_test"], 
       d[d$gender == "Masculino", "AQ_test"])