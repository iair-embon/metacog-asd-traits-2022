###############################################
### Linear Regression Analysis AQ Subscales ### TAB 3
###############################################

require(gtsummary)
require(dplyr)

# data
root <- rprojroot::is_rstudio_project
basename(getwd())               
filepath <- root$find_file("Data/Regression_Results/AQ_subscales_linear_model.RData")
load(file= filepath)

table3 <- a %>%
  tbl_regression(
               intercept = T,
               pvalue_fun = ~style_pvalue(.x, digits = 3),
               estimate_fun =  ~style_number (.x, digits = 3),
               label = list(
                 "(Intercept)" ~ "Intercept",
                 "AQ_social.norm" ~ "Social Skill.norm",
                 "AQ_AttentionSwitch.norm" ~ "Attention Switching.norm",
                 "AQ_AttentionDetail.norm" ~ "Attention to Detail.norm",
                 "AQ_communication.norm" ~ "Communication.norm",
                 "AQ_imagination.norm" ~ "Imagination.norm",
                 "gender" ~ "Gender[m]",
                 "age.norm" ~ "Age.norm")
               ) %>%
  modify_header(label ~ "") %>%
  modify_column_unhide(column = std.error) %>%
  add_global_p() %>%
  bold_p(t = 0.05) %>%
  add_glance_table(include = c(r.squared, adj.r.squared))

gt::gtsave(as_gt(table3), file = "Tables/AQ_subscales_linear_model.png")

