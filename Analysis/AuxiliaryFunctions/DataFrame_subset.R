DataFrame_subset <- function(df_total){ 
  
  ExistingSubjects <- unique(df_total$Participant)
  
  # Variables of interest
  PC <- rep(NaN, length(ExistingSubjects))
  mc <- rep(NaN, length(ExistingSubjects))
  gender <- rep(NaN, length(ExistingSubjects))
  AQ_test <- rep(NaN, length(ExistingSubjects))
  AQ_social <- rep(NaN, length(ExistingSubjects))
  AQ_AttentionSwitch <- rep(NaN, length(ExistingSubjects))
  AQ_AttentionDetail <- rep(NaN, length(ExistingSubjects))
  AQ_communication <- rep(NaN, length(ExistingSubjects))
  AQ_imagination <- rep(NaN, length(ExistingSubjects))
  age <- rep(NaN, length(ExistingSubjects))
  ConfMean <- rep(NaN, length(ExistingSubjects))

  for (i in 1:length(ExistingSubjects)) { 
    PC[i] <- unique(df_total[df_total$Participant == ExistingSubjects[i],"PC"])
    mc[i] <- unique(df_total[df_total$Participant == ExistingSubjects[i],"mc"])
    gender[i]<- unique(df_total[df_total$Participant == ExistingSubjects[i],"gender"]) 
    AQ_test[i]<- unique(df_total[df_total$Participant == ExistingSubjects[i],"AQ_test"])
    AQ_social[i]<- unique(df_total[df_total$Participant == ExistingSubjects[i],"AQ_social"])
    AQ_AttentionSwitch[i]<- unique(df_total[df_total$Participant == ExistingSubjects[i],"AQ_AttentionSwitch"])
    AQ_AttentionDetail[i]<- unique(df_total[df_total$Participant == ExistingSubjects[i],"AQ_AttentionDetail"])
    AQ_communication[i]<- unique(df_total[df_total$Participant == ExistingSubjects[i],"AQ_communication"])
    AQ_imagination[i]<- unique(df_total[df_total$Participant == ExistingSubjects[i],"AQ_imagination"])
    age[i]<- unique(df_total[df_total$Participant == ExistingSubjects[i],"age"])
    ConfMean[i]<- unique(df_total[df_total$Participant == ExistingSubjects[i],"ConfMean"])
    }
  
  d = data.frame(Participant = ExistingSubjects,
                 mc  = mc,
                 gender = gender, 
                 PC  = PC,
                 AQ_test = AQ_test,
                 AQ_social = AQ_social,
                 AQ_AttentionSwitch = AQ_AttentionSwitch,
                 AQ_AttentionDetail = AQ_AttentionDetail,
                 AQ_communication = AQ_communication,
                 AQ_imagination = AQ_imagination,
                 age = age,
                 m_c = ConfMean)
  
  return(d)
}