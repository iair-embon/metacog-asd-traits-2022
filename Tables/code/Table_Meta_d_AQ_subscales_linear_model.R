######################################################
### Linear Regression Analysis AQ Subscales Meta d ### 
######################################################

require(gtsummary)
require(dplyr)
library(webshot2)

# data
root <- rprojroot::is_rstudio_project
basename(getwd())               
filepath <- root$find_file("Data/Regression_Results/All_participants_Non_Exclusion/Meta_d_AQ_subscales_linear_model.RData")
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

gt::gtsave(as_gt(table3), file = "Tables/Tables/All_participants_Non_Exclusion/Meta_d_AQ_subscales_linear_model.png")

# data model 2
filepath <- root$find_file("Data/Regression_Results/All_participants_Non_Exclusion/Meta_d_AQ_subscales_linear_model_2.RData")
load(file= filepath)

table3_2 <- a2 %>%
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
      "AQ_imagination.std" ~ "Imagination.std")
  ) %>%
  modify_header(label ~ "") %>%
  modify_column_unhide(column = std.error) %>%
  add_global_p() %>%
  add_q() %>%
  bold_p(t = 0.05, q = TRUE) %>%
  add_glance_table(include = c(r.squared, adj.r.squared))

gt::gtsave(as_gt(table3_2), file = "Tables/Tables/All_participants_Non_Exclusion/Meta_d_AQ_subscales_linear_model_2.png")


