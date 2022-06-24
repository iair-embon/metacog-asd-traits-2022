##########################################
### Mixed Logistic Regression Analysis ### TAB 2
##########################################

require(gtsummary)
require(dplyr)
require(broom.mixed)

# data
### load mixed logistic regression model 
root <- rprojroot::is_rstudio_project
basename(getwd())               
filepath <- root$find_file("Data/Regression_Results/MixedLogisticRegressionAnalysis.RData")
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
      "confidence_key.norm:AQ_test.norm" ~ "AQ.norm:Confidence.norm",
      "confidence_key.norm:gender" ~ "Gender[m]:Confidence.norm",
      "confidence_key.norm:age.norm" ~ "Age.norm:Confidence.norm",
      "confidence_key.norm:AQ_test.norm:gender" ~ "AQ.norm:Gender[m]:Confidence.norm",
      "confidence_key.norm:AQ_test.norm:age.norm" ~ "AQ.norm:Age.norm:Confidence.norm",
      "Participant.sd__(Intercept)" ~ "Subjects SE(Intercept)")) %>%
  modify_header(label ~ "") %>%
  modify_column_unhide(column = std.error) %>%
  add_global_p() %>%
  bold_p(t = 0.05) %>%
  add_glance_table(include = deviance)

gt::gtsave(as_gt(table2), file = "Tables/MixedLogisticRegressionAnalysis.png")