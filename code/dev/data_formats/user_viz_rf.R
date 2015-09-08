## Data processing format for user based Random Forrest ##
setwd('/media/hdd/kaggle/recruit/data')


getMe <- function(){

  ## Allow me to split the training and testing data
  coup_list_train$HOLD <- TRUE
  coup_list_test$HOLD <- FALSE
  
  all_data <- rbind(coup_list_train, coup_list_test)
  all_data[is.na(all_data)] <- 1 #assume NA usable period is anytime
  
  # Remove data that is otherwise represented:
  all_data$USE_WEEKDAY <- sum(all_data$USABLE_DATE_MON, 
                              all_data$USABLE_DATE_TUE, 
                              all_data$USABLE_DATE_WED, 
                              all_data$USABLE_DATE_THU, 
                              all_data$USABLE_DATE_FRI, 
                              all_data$USABLE_DATE_SAT, 
                              all_data$USABLE_DATE_SUN)
  
  all_data$USE_WEEKEND <- sum(all_data$USABLE_DATE_SAT, 
                              all_data$USABLE_DATE_SUN)
  
  for_removal <- c('DISPFROM', 'DISPEND', 'VALIDFROM', 'VALIDEND',
                   'USABLE_DATE_MON',
                   'USABLE_DATE_TUE',
                   'USABLE_DATE_WED',
                   'USABLE_DATE_THU',
                   'USABLE_DATE_FRI',
                   'USABLE_DATE_SAT',
                   'USABLE_DATE_SUN')
  
  remove_me <- match(for_removal, names(all_data))
  
  all_data <- all_data[, -remove_me]
  
  ## EXPAND ALL DATA ##
  supporting_cols_num <- which(names(all_data) %in% 
                                 c('COUPON_ID_hash', 'HOLD'))
  supporting_cols <- all_data[, supporting_cols_num]
  
  factors <- which(sapply(all_data[, -supporting_cols_num], is.factor)==TRUE)
  request_contrasts <- lapply(all_data[,names(factors)], contrasts, contrasts=FALSE)
  expanded <- model.matrix(~ -1 + ., all_data[, -supporting_cols_num], contrasts.arg= request_contrasts)
  expanded <- as.data.frame(expanded)
  
  all_training_data <<- expanded[supporting_cols$HOLD == TRUE, ]
  all_training_coupon_ids <<- supporting_cols$COUPON_ID_hash[supporting_cols$HOLD == TRUE]
  
  all_testing_data <<- expanded[supporting_cols$HOLD == FALSE, ]
  all_testing_coupon_ids <<- supporting_cols$COUPON_ID_hash[supporting_cols$HOLD == FALSE]
}

getMe()
