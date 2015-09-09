data_formats <- '/media/hdd/kaggle/recruit/code/dev/data_formats'

# Data Files used throughout all data processes ######
coup_det_train <- read.csv("coupon_detail_train.csv", header = T, stringsAsFactors = F)
coup_visit_train <- read.csv("coupon_visit_train.csv", header = T, stringsAsFactors = F)
coup_det_train$PURCHASE_FLG <- 1

coup_list_train <- read.csv("coupon_list_train.csv", header = T, stringsAsFactors = T)
coup_list_test <- read.csv("coupon_list_test.csv", header = T, stringsAsFactors = T)

user_list <- read.csv("user_list.csv")

##########



produceGT <- function(DATA){
  ground_truth <- as.data.frame(aggregate(.~USER_ID_hash, data=DATA,FUN=paste))
  return(ground_truth)
}

## MAIN data fetching function ###
getData <- function(TYPE, HOP_ST='NONE', HOP_ED='NONE'){
  
  if (TYPE == 'INFO'){print(c('user_viz_rf', 'cos_sim_ser'))}
  if (TYPE == 'user_viz_rf'){source(paste(data_formats, '/', TYPE, '.R', sep = ''))}
  if (TYPE == 'cos_sim_ser'){source(paste(data_formats, '/', TYPE, '.R', sep = ''))}
  
  if (HOP_ST != 'NONE' & !is.na(as.Date(HOP_ST, format = '%Y-%m-%d'))){
    
    HOP_ST <- as.Date(HOP_ST, format = '%Y-%m-%d')
    HOP_ED <- as.Date(HOP_ED, format = '%Y-%m-%d')
    
    global_holdout <- coup_list_train$COUPON_ID_hash[
      HOP_ST <= as.Date(coup_list_train$DISPFROM) & 
      HOP_ED >= as.Date(coup_list_train$DISPFROM)]
    
  }
  
  # Create Validation Set with coupons removed
  validation_coupon_index <<- match(global_holdout, all_training_coupon_ids)
  formatted_validation_set <<- all_training_data[validation_coupon_index, ]
  val_removed_training_data <<- all_training_data[-c(validation_coupon_index), ]
  map_ready_ground_truth <<- produceGT(coup_det_train[coup_det_train$COUPON_ID_hash %in% global_holdout
                                                     , c('USER_ID_hash', 'COUPON_ID_hash')]) # givena list of coupons I say a user will purchase them
  
  names(map_ready_ground_truth) <- c('USER_ID_hash', 'PURCHASED_COUPONS')
  
}

## Process the Visit Data ##
# Generally Cobmine Visit data and purchase data then roll to the coupon level

getUserTraining <- function(USER){
  
  is_purch <- as.data.frame(subset(coup_det_train[, c('COUPON_ID_hash', 'PURCHASE_FLG')],
                     coup_det_train$USER_ID_hash == USER))
  
  is_purch_or_vis <- as.data.frame(subset(coup_visit_train[, c(1, 5)],
                            coup_visit_train$USER_ID_hash == USER))
  iporv <- is_purch_or_vis[, c('VIEW_COUPON_ID_hash', 'PURCHASE_FLG')]
  names(iporv) <- names(is_purch)
  
  coup_dat <- rbind(is_purch, iporv)
  coupon_targets <- aggregate(.~COUPON_ID_hash, data=coup_dat,FUN=max)
  return(coupon_targets)
  
}


