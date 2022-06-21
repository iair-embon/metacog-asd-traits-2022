##################
#  preprocessing #
##################

root <- rprojroot::is_rstudio_project
basename(getwd())               

# load dataframe 
filepath <- root$find_file("Data/df_total.Rda")
load(file= filepath)

### Exclusion criteria, data is excluded of future analysis

# leaving only those who tell us that we can count on their answers.
library (stringr)
library (tidyverse)
df_total <- df_total %>% 
  filter(str_detect(df_total$RelyOn, "Pueden")) # if start with "Pueden"
                                                      # it stays

# leaving only those who did not interrup the 
# task drastically (= ok)
df_total <- df_total[df_total$Problems == 'ok',] 

# Filter by performance, leaving only those who have PC > 60 
df_total <- df_total[df_total$PC > 0.60,]

#  having pressed the same confidence key more than 85% of trials
source(root$find_file("Analysis/AuxiliaryFunctions/discard_by_x_same_confidence_new.R"))
sujetos_a_descartar <- discard_by_x_same_confidence_new(85,df_total)  
df_total <- df_total[! df_total$Participant %in% sujetos_a_descartar,]

# Filter by reaction times
df_total <- df_total[df_total$TimeDiscTrial <= 5000,]
df_total <- df_total[df_total$TimeDiscTrial >= 200,]
df_total <- df_total[df_total$TimeConfTrial <=5000,]

# burning the first 20 trials of each subject
df_total <- df_total[df_total$trial > 20,]

# Filter by trails needed to calculate AUROC2
# discarding because very few trials
cant_trials_por_sujeto <- rep(NaN, length(unique(df_total$Participant)))
existing_subject <- unique(df_total$Participant)

for (i in 1:length(cant_trials_por_sujeto)) {
  cant_trials_por_sujeto[i] <- nrow(df_total[df_total$Participant == existing_subject[i],])
}

# I see who are the ones who have fewer trials than X
indices_cant_trials <- which(cant_trials_por_sujeto < 90)
subj_pocos_trials<- existing_subject[indices_cant_trials]

# I discard them
df_total <- df_total[! df_total$Participant %in% subj_pocos_trials,]

### AUROC2
# get metacognitive sensivity

# load the type 2 ROC analysis function
source(root$find_file("Analysis/AuxiliaryFunctions/auroc2.R"))

Nsuj <- length(unique(df_total$Participant))
# saving metacog = mc for each RT discarded
mc <- rep(NA, Nsuj)
ExistingSubjects <- unique(df_total$Participant)

for (i in 1:Nsuj){
  mc[i] <- type2roc(correct = df_total$discrimination_is_correct[df_total$Participant==ExistingSubjects[i]],
                    conf = df_total$confidence_key[df_total$Participant==ExistingSubjects[i]], 
                    Nratings = 4)}

## adding column mc to df_total

todos_sujetos_mc <- c()

for (i in 1:length(ExistingSubjects)) {
  
  sujeto_df_exp <- df_total[df_total$Participant == ExistingSubjects[i],]
  cant_trials <- nrow(sujeto_df_exp)
  
  sujeto_mc <-rep(mc[i],cant_trials)
  
  todos_sujetos_mc <- c(todos_sujetos_mc,sujeto_mc)
}

df_total$mc <- todos_sujetos_mc

# filter those who have an AUROC2 less than 1.5 standard deviations from the mean
mean_mc <- mean(mc)
sd_mc <-sd(mc)
df_total <- df_total[df_total$mc >= mean_mc - sd_mc* 1.5,]

### Save the df_total, now df_total_filtered
filepath <- root$find_file("Data/df_total_filtered.Rda")
save(df_total,file = filepath)

