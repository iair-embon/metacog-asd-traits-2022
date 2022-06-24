#########################################
### Linear Regression Analysis AUROC2 ### TAB 1
#########################################

require(gtsummary)
require(dplyr)

# data
root <- rprojroot::is_rstudio_project
basename(getwd())               
filepath <- root$find_file("Data/Regression_Results/AUROC2_AQ_linear_model.RData")
load(file= filepath)

table1 <- a %>%
  tbl_regression(
               intercept = T,
               pvalue_fun = ~style_pvalue(.x, digits = 3),
               estimate_fun =  ~style_number (.x, digits = 3),
               label = list(
                 "(Intercept)" ~ "Intercept",
                 "AQ_test.norm" ~ "AQ.norm",
                 "gender" ~ "Gender[m]",
                 "age.norm" ~ "Age.norm",
                 "AQ_test.norm:gender" ~ "AQ.norm:Gender[m]",
                 "AQ_test.norm:age.norm" ~ "AQ.norm:Age.norm")
               ) %>%
  modify_header(label ~ "") %>%
  modify_column_unhide(column = std.error) %>%
  add_global_p() %>%
  bold_p(t = 0.05) %>%
  add_glance_table(include = c(r.squared, adj.r.squared))

gt::gtsave(as_gt(table1), file = "Tables/AUROC2_linear_model.png")

