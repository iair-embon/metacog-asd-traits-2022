# load two packages required to to grid plots
require("cowplot")
require("gridGraphics")
require("tidyverse")


#### A: confidence distribution for a hypothetical low metacognition participant####

# data
d.low.metacog <- matrix(c(0.26, 0.245, 0.24, 0.255, 0.255, 0.24, 0.245, 0.26), 
                        nrow = 2, 
                        ncol = 4, 
                        byrow = T, 
                        dimnames = list(c("incorrect trials", "correct trials"), 
                                        c("Low","","","High")))

# plot
pA <- ~{
  barplot(height = d.low.metacog,                      
          beside = TRUE,
          ylim = c(0,0.5),
          xlab = "confidence",
          cex.lab = 1.5,
          col = c("gray", "black"))
  
  legend(4,0.65,
         xpd = T,
         legend = rownames(d.low.metacog),
         pch = 15,
         col = c("gray", "black"),
         bty = "n")
  
  points(x = c(3.5, 6.5, 9.5), c(0.45, 0.45, 0.45), pch = c(18, 15, 16), cex = 2)
  lines(x = c(3.5,3.5), y = c(0,0.4), lw = 2, lty = 2)
  lines(x = c(6.5,6.5), y = c(0,0.4), lw = 2, lty = 2)
  lines(x = c(9.5,9.5), y = c(0,0.4), lw = 2, lty = 2)
}

#### B: confidence distribution for a hypothetical high metacognition participant  ####

# data
d.high.metacog <- matrix(c(0.45, 0.3, 0.15, 0.1, 0.05, 0.15, 0.30, 0.50), 
                         nrow = 2, 
                         ncol = 4, 
                         byrow = T,
                         dimnames = list(c("incorrect trials", "correct trials"), 
                                         c("Low","","","High")))

# plot
pB <- ~{
  barplot(height = d.high.metacog,                      
          beside = TRUE,
          ylim = c(0,0.8),
          xlab = "confidence",
          cex.lab=1.5,
          col = c("gray", "black"))
  
  legend(4,1.06,
         xpd = T,
         legend = rownames(d.high.metacog),
         pch = 15,
         col = c("gray", "black"),
         bty = "n")
  
  points(x = c(3.5, 6.5, 9.5), c(0.75, 0.75, 0.75), pch = c(18, 15, 16), cex = 2)
  lines(x = c(3.5,3.5), y = c(0,0.65), lw = 2, lty = 2)
  lines(x = c(6.5,6.5), y = c(0,0.65), lw = 2, lty = 2)
  lines(x = c(9.5,9.5), y = c(0,0.65), lw = 2, lty = 2)
}



#### C: Type2-ROC for the low metacognition hypothetical participant ####
roc2.low.metacog = data.frame(pinc = c(0, cumsum(rev(d.low.metacog[1,]))),
                              pcor = c(0, cumsum(rev(d.low.metacog[2,]))))


pC <- ~{
  par(pty="s")
  plot(pcor ~ pinc, data=roc2.low.metacog, 
       type = "l", 
       xlim = c(0,1),
       ylim = c(0,1),
       lwd = 2,
       xlab = "p(confidence|incorrect)",
       ylab = "p(confidence|correct)",
       cex.lab = 1.2 )
  points(x = roc2.low.metacog$pinc[2:4], 
         y = roc2.low.metacog$pcor[2:4],
         pch = c(18, 15, 16), 
         cex = 2)
  abline(0, 1, lwd = 2, lty = 3)
}

#### D: Type2-ROC for the high metacognition hypothetical participant ####

roc2.high.metacog = data.frame(pinc = c(0, cumsum(rev(d.high.metacog[1,]))),
                               pcor = c(0, cumsum(rev(d.high.metacog[2,]))))


pD <- ~{
  par(pty="s")
  plot(pcor ~ pinc, data=roc2.high.metacog, 
       type = "l", 
       xlim = c(0,1),
       ylim = c(0,1),
       lwd = 2,
       xlab = "p(confidence|incorrect)",
       ylab = "p(confidence|correct)",
       cex.lab = 1.2 )
  points(x = roc2.high.metacog$pinc[2:4], 
         y = roc2.high.metacog$pcor[2:4],
         pch = c(18, 15, 16), 
         cex = 2)
  abline(0, 1, lwd = 2, lty = 3)
}

#### E: Logistic regression for the low metacognition hypothetical participant ####

d.low.metacog.df <- as.data.frame(d.low.metacog) %>%
  mutate(Correct = c(0,1)) %>%
  pivot_longer(c(Low,V2,V3,High)) %>%
  mutate(confidence = rep(c(1,2,3,4),2)) %>%
  slice_sample(n = 130, weight_by = value, replace = T) 

p_low <- ggplot(d.low.metacog.df, aes(confidence,Correct)) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE, color = "black") +
  scale_y_continuous("p(correct)" ,
                     breaks = seq(0,1, length.out = 4),
                     labels = round(seq(0.5,1, length.out = 4), digits = 2),
                     limits = c(0,1))+
  xlab("confidence") +
  expand_limits(y = 0)+ 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.title.x=element_text(size = 15),
        axis.text.x=element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 15))


#### F: Logistic regression for the high metacognition hypothetical participant ####

d.high.metacog.df <- as.data.frame(d.high.metacog) %>%
  mutate(Correct = c(0,1)) %>%
  pivot_longer(c(Low,V2,V3,High)) %>%
  mutate(confidence = rep(c(1,2,3,4),2)) %>%
  slice_sample(n = 130, weight_by = value, replace = T) 

p_high <- ggplot(d.high.metacog.df, aes(confidence,Correct)) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE, color = "black") +
  scale_y_continuous("p(correct)" ,
                     breaks = seq(0,1, length.out = 4),
                     labels = round(seq(0.5,1, length.out = 4), digits = 2),
                     limits = c(0,1))+
  xlab("confidence") +
  expand_limits(y = 0)+ 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.title.x=element_text(size = 15),
        axis.text.x=element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 15))




#### Grid plot ####

g <- plot_grid(pA, pC, p_low, pB, pD, p_high, 
          labels = c('A', 'C', 'E', 'B', 'D', 'F'),
          label_size = 24)


ggsave("Figures/Figures/2.png", g,
       width = 10, height = 8, bg = "white")