# having pressed the same confidence key more than X% of trials

discard_by_x_same_confidence_new <- function(percent,df_total){

  Conf1 <- rep(NaN, length(unique(df_total$Participant)))
  Conf2 <- rep(NaN, length(unique(df_total$Participant)))
  Conf3 <- rep(NaN, length(unique(df_total$Participant)))
  Conf4 <- rep(NaN, length(unique(df_total$Participant)))
  existing_subject <- unique(df_total$Participant)
  
  for (i in 1:length(existing_subject)) {
    Conf1[i] <- unique(df_total[df_total$Participant == existing_subject[i],'ConfKey1'])
    Conf2[i] <- unique(df_total[df_total$Participant == existing_subject[i],'ConfKey2'])
    Conf3[i] <- unique(df_total[df_total$Participant == existing_subject[i],'ConfKey3'])
    Conf4[i] <- unique(df_total[df_total$Participant == existing_subject[i],'ConfKey4'])
  }
  
  df_conf <- data_frame(Participant = existing_subject,
                        Conf1 = Conf1,
                        Conf2 = Conf2,
                        Conf3 = Conf3,
                        Conf4 = Conf4)
  
  existing_subject <- unique(df_total$Participant)
  
  filtro <- round((percent*130)/100)
  discard_participant <- c()
  j <-1
  for (i in 1:nrow(df_conf)) {
    discard_p <- FALSE
    df_subj <- df_conf[df_conf$Participant == existing_subject[i],]
    if(df_subj$Conf1 > filtro){discard_p <- TRUE}
    if(df_subj$Conf2 > filtro){discard_p <- TRUE}
    if(df_subj$Conf3 > filtro){discard_p <- TRUE}
    if(df_subj$Conf4 > filtro){discard_p <- TRUE}
    
    if(discard_p == TRUE){
      discard_participant <- c(discard_participant, existing_subject[i])
      j <- j+1}
  }
  
  return(discard_participant)
}
