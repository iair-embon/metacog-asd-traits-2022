#####################
### AQ histograms ### FIG 3b
#####################

require(tidyverse)
require(cowplot)

root <- rprojroot::is_rstudio_project
basename(getwd())               

####### data frame with filters already applied
filepath <- root$find_file("Data/df_total_filtered.Rda")
load(file= filepath)

source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_subset.R"))
d <- DataFrame_subset(df_total)

d[d == "Masculino"] <- 'Male' 
d[d == "Femenino"] <- 'Female' 

# female
gF <- ggplot(d, aes(x = AQ_test))+
  geom_bar(data=subset(d, gender == 'Female'), fill = "black")+
  scale_x_continuous(expand = expansion(mult = c(0, 0)),limits = c(0, 40)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  xlab("AQ") +
  ylab("Numbrer of female participants")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        strip.text = element_text(size = 20),
        axis.title.x = element_text(size = 30))

# male
gM <- ggplot(d, aes(x = AQ_test))+
  geom_bar(data=subset(d, gender == 'Male'), fill = "black")+
  scale_x_continuous(expand = expansion(mult = c(0, 0)),limits = c(0, 40)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  xlab("AQ") +
  ylab("Numbrer of male participants")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30))

g <- plot_grid(gF, gM, 
               labels = c('B', ''),
               label_size = 24)


ggsave("Figures/Figures/3b.png", g,
       width = 14, height = 8, bg = "white")
