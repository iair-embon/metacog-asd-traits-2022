##########################################
### Mixed Logistic Regression Analysis ### TAB 2
##########################################

require(gtsummary)
require(dplyr)
require(broom.mixed)
library(webshot2)

# data
### load mixed logistic regression model 
root <- rprojroot::is_rstudio_project
basename(getwd())               
filepath <- root$find_file("Data/Regression_Results/Non_Exclusion_Criteria/MixedLogisticRegressionAnalysis.RData")
load(file= filepath)

table2 <- a_log %>%
  tbl_regression(
    intercept = T,
    pvalue_fun = ~style_pvalue(.x, digits = 3),
    estimate_fun =  ~style_ratio (.x, digits = 3),
    tidy_fun = broom.mixed::tidy,
    label = list(
      "(Intercept)" ~ "Intercept",
      "confidence_key.norm" ~ "Confidence.norm",
      "confidence_key.norm:AQ_test.std" ~ "AQ.std:Confidence.norm",
      "confidence_key.norm:gender" ~ "Gender[m]:Confidence.norm",
      "confidence_key.norm:age.std" ~ "Age.std:Confidence.norm",
      "confidence_key.norm:AQ_test.std:gender" ~ "AQ.std:Gender[m]:Confidence.norm",
      "confidence_key.norm:AQ_test.std:age.std" ~ "AQ.std:Age.std:Confidence.norm",
      "Participant.sd__(Intercept)" ~ "Subjects SE(Intercept)")) %>%
  modify_header(label ~ "") %>%
  modify_column_unhide(column = std.error) %>%
  add_q() %>%
  bold_p(t = 0.05, q = TRUE) %>%
  add_glance_table(include = deviance)

gt::gtsave(as_gt(table2), file = "Tables/Tables/Non_Exclusion_Criteria/MixedLogisticRegressionAnalysis.png")

# data model 2
### load mixed logistic regression model 
filepath <- root$find_file("Data/Regression_Results/Non_Exclusion_Criteria/MixedLogisticRegressionAnalysis_2.RData")
load(file= filepath)

table2_2 <- a_log2 %>%
  tbl_regression(
    intercept = T,
    pvalue_fun = ~style_pvalue(.x, digits = 3),
    estimate_fun =  ~style_ratio (.x, digits = 3),
    tidy_fun = broom.mixed::tidy,
    label = list(
      "(Intercept)" ~ "Intercept",
      "confidence_key.norm" ~ "Confidence.norm",
      "confidence_key.norm:AQ_test.std" ~ "AQ.std:Confidence.norm",
      "Participant.sd__(Intercept)" ~ "Subjects SE(Intercept)")) %>%
  modify_header(label ~ "") %>%
  modify_column_unhide(column = std.error) %>%
  add_global_p() %>%
  add_q() %>%
  bold_p(t = 0.05, q = TRUE) %>%
  add_glance_table(include = deviance)

gt::gtsave(as_gt(table2_2), file = "Tables/Tables/Non_Exclusion_Criteria/MixedLogisticRegressionAnalysis_2.png")
