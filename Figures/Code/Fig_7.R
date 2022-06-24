###################################
### Regression model confidence ### FIG 7
###################################

require(tidyverse)
require(jtools)
require(broom.mixed)

### load linear regression model 
root <- rprojroot::is_rstudio_project
basename(getwd())               
filepath <- root$find_file("Data/Regression_Results/ConfidenceMean_AQ_linear_model.RData")
load(file= filepath)

plot_summs(a, coefs = c('AQ' = 'AQ_test.norm',
                        'Gender[m]'='gender',
                        'Age' = 'age.norm',
                        'AQ:Gender[m]'='AQ_test.norm:gender',
                        'AQ:Age'='AQ_test.norm:age.norm'),
           colors = "black")+
  ylab("") +
  xlab("Regression coefficient") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25), 
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25))

ggsave("Figures/Figures/7a.png", 
       width = 10, height = 6)

#### regression line and scatter plot

# data frame with filters already applied
filepath <- root$find_file("Data/df_total_filtered.Rda")
load(file= filepath)

source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_subset.R"))
d <- DataFrame_subset(df_total)

d$m_c.norm <- (d$m_c - mean(d$m_c))/ sd(d$m_c)

# convert the normalized AQ scores to the original scores
intercept <- coefficients(a)[[1]]
slope <- coefficients(a)[[2]]

converted_intercept <- intercept-slope*mean(d$AQ_test)/sd(d$AQ_test)
converted_slope <- slope/sd(d$AQ_test)

# figure
ggplot(d, aes(x=AQ_test, y=m_c.norm)) + 
  geom_point()+
  geom_abline(intercept = converted_intercept, 
              slope = converted_slope)+
  ylab("Confidence mean") +
  xlab("AQ") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.title.x=element_text(size = 30),
        axis.text.x=element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30))

ggsave("Figures/Figures/7b.png", 
       width = 10, height = 6)