##########################################
### Linear Regression Analysis M ratio ### 
##########################################

require(gtsummary)
require(dplyr)
library(webshot2)

# data
root <- rprojroot::is_rstudio_project
basename(getwd())               
filepath <- root$find_file("Data/Regression_Results/Non_Exclusion_Criteria/M_ratio_AQ_linear_model.RData")
load(file= filepath)

table1 <- a %>%
  tbl_regression(
               intercept = T,
               pvalue_fun = ~style_pvalue(.x, digits = 3),
               estimate_fun =  ~style_number (.x, digits = 3),
               label = list(
                 "(Intercept)" ~ "Intercept",
                 "AQ_test.std" ~ "AQ.std",
                 "gender" ~ "Gender[m]",
                 "age.std" ~ "Age.std",
                 "AQ_test.std:gender" ~ "AQ.std:Gender[m]",
                 "AQ_test.std:age.std" ~ "AQ.std:Age.std")
               ) %>%
  modify_header(label ~ "") %>%
  modify_column_unhide(column = std.error) %>%
  add_global_p() %>%
  add_q() %>%
  bold_p(t = 0.05, q = TRUE) %>%
  add_glance_table(include = c(r.squared, adj.r.squared))

gt::gtsave(as_gt(table1), file = "Tables/Tables/Non_Exclusion_Criteria/M_ratio_AQ_linear_model.png")

## data model 2
filepath <- root$find_file("Data/Regression_Results/Non_Exclusion_Criteria/M_ratio_AQ_linear_model_2.RData")
load(file= filepath)

table1_2 <- a2 %>%
  tbl_regression(
    intercept = T,
    pvalue_fun = ~style_pvalue(.x, digits = 3),
    estimate_fun =  ~style_number (.x, digits = 3),
    label = list(
      "(Intercept)" ~ "Intercept",
      "AQ_test.std" ~ "AQ.std")
  ) %>%
  modify_header(label ~ "") %>%
  modify_column_unhide(column = std.error) %>%
  add_global_p() %>%
  add_q() %>%
  bold_p(t = 0.05, q = TRUE) %>%
  add_glance_table(include = c(r.squared, adj.r.squared))

gt::gtsave(as_gt(table1_2), file = "Tables/Tables/Non_Exclusion_Criteria/M_ratio_AQ_linear_model_2.png")
