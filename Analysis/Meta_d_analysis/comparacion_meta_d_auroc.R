library(tidyverse)

# load Meta data
root <- rprojroot::is_rstudio_project
basename(getwd())

meta_d <- read.table("Analysis/Meta_d_analysis/meta_d.txt")
M_ratio <- read.table("Analysis/Meta_d_analysis/M_ratio.txt")
M_diff <- read.table("Analysis/Meta_d_analysis/M_diff.txt")

# load usual df with AUROC2
filepath <- root$find_file("Data/df_total_filtered.Rda")
load(file= filepath)

source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_subset.R"))
d <- DataFrame_subset(df_total_filtered)

# Run a linear regression, predicting AUROC2 with meta_d
summary(lm(d$mc ~ meta_d$V1))

# ploting the predicting line using scatter plot
df <- data.frame(meta_d = meta_d$V1,
                 auroc = d$mc)

ggplot(df, aes(x= meta_d, y=auroc))+
  geom_point() +
  geom_smooth(method = "lm")

# Now the same with the M_ratio (metacognitive efficiency)

# Run a linear regression, predicting AUROC2 with meta_d
summary(lm(d$mc ~ M_ratio$V1))

# ploting the predicting line using scatter plot
df <- data.frame(M_ratio = M_ratio$V1,
                 auroc = d$mc)

ggplot(df, aes(x= M_ratio, y=auroc))+
  geom_point() +
  geom_smooth(method = "lm")

