###############################################
### Linear Regression Analysis AQ Subscales ### TAB 3
###############################################

require(gtsummary)
require(dplyr)
library(webshot2)

# data
root <- rprojroot::is_rstudio_project
basename(getwd())               
filepath <- root$find_file("Data/Regression_Results/Non_Exclusion_Criteria/AUROC2_AQ_subscales_linear_model.RData")
load(file= filepath)

table3 <- a %>%
  tbl_regression(
               intercept = T,
               pvalue_fun = ~style_pvalue(.x, digits = 3),
               estimate_fun =  ~style_number (.x, digits = 3),
               label = list(
                 "(Intercept)" ~ "Intercept",
                 "AQ_social.std" ~ "Social Skill.std",
                 "AQ_AttentionSwitch.std" ~ "Attention Switching.std",
                 "AQ_AttentionDetail.std" ~ "Attention to Detail.std",
                 "AQ_communication.std" ~ "Communication.std",
                 "AQ_imagination.std" ~ "Imagination.std",
                 "gender" ~ "Gender[m]",
                 "age.std" ~ "Age.std")
               ) %>%
  modify_header(label ~ "") %>%
  modify_column_unhide(column = std.error) %>%
  add_global_p() %>%
  add_q() %>%
  bold_p(t = 0.05, q = TRUE) %>%
  add_glance_table(include = c(r.squared, adj.r.squared))

gt::gtsave(as_gt(table3), file = "Tables/Tables/Non_Exclusion_Criteria/AUROC2_AQ_subscales_linear_model.png")