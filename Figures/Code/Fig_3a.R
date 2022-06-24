#############################
### AUROC2 vs Performance ### FIG 3a
#############################

require(tidyverse)
require(ggtext)

root <- rprojroot::is_rstudio_project
basename(getwd())               

####### data frame with filters already applied
filepath <- root$find_file("Data/df_total_filtered.Rda")
load(file= filepath)

source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_subset.R"))
d <- DataFrame_subset(df_total)

mc.sorted <-  d[order(d$mc),]
subjects <- 1:nrow(mc.sorted)
mc.sorted$s <- subjects

mc.sorted <- mc.sorted %>% 
  select(mc,PC,s) %>% 
  pivot_longer(!s)

ggplot(mc.sorted, aes(x=s,y=value,color=name)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("black","grey"),labels = c("Metacognition", "Performance"))+
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.1))) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        legend.text =  element_text(size = 25),
        legend.position = c(0.7, 0.2),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave("Figures/Figures/3a.png", 
       width = 10, height = 6)
