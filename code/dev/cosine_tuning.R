#setwd('/media/hdd/kaggle/recruit/data')
#source('/media/hdd/kaggle/recruit/code/dev/data_process.R')
#source('/media/hdd/kaggle/recruit/code/dev/evaluation-mapk.R')
setwd('/home/ubuntu/JOE/data')
source('/home/ubuntu/JOE/recruit/code/dev/data_process.R')
source('/home/ubuntu/JOE/recruit/code/dev/evaluation-mapk.R')

library(doParallel) # Used to compute the user matrix
library(data.table) # Used to Rbind the results 
library(glmnet)
library(Matrix)

DT_ST <- as.Date('2011-07-01', format = '%Y-%m-%d')
DT_ED <- as.Date('2012-06-23', format = '%Y-%m-%d')

weeks <- as.character(c(seq(DT_ST, DT_ED, 6), DT_ED))
total <- length(weeks)
wk_pairs <- list()

for (i in 1:(length(weeks)-1)){
  wk_pairs[[i]] <- c(weeks[i], weeks[i+1])
}

W <- as.matrix(Diagonal(x=c(rep(2,13), rep(2,1), rep(-0.13,1), rep(0,9), rep(0.51,9), rep(1,47), rep(4.78,55))))#calculation of cosine similairties of users and coupons
subset <- as.character(clusterlookup[clusterlookup$cluster == "f1","USER_ID_hash"])
tuner <- function(){
  for (WK in 1:length(wk_pairs)){
    
    ## GET STATING STANDARD DATA #
    getData('cos_sim_ser', wk_pairs[[WK]][1], wk_pairs[[WK]][2])
    
    source('/media/hdd/kaggle/recruit/code/dev/model_process_cst.R')
#    source('/home/ubuntu/JOE/recruit/code/dev/model_process_cst.R')   
    # validation_coupon_index
    # formatted_validation_set
    # val_removed_training_data
    # map_ready_ground_truth
    # all_training_data
    # all_training_coupon_ids
    # all_testing_data
    # all_testing_coupon_ids
    
    sample_iterations <- 60
    try(feature_weights <- mclapply(1:sample_iterations, cosine_tuner2, R_TARGETS, R_NT, mc.cores = 7))
    try(D <- return_weights(feature_weights, paste('V', 1:ncol(A), sep='')))
    
    
    # Get cosine sim predictions
    score = as.matrix(A) %*% D %*% t(as.matrix(B))
    #order the list of coupons according to similairties and take only first 10 coupons
    PURCHASED_COUPONS <- do.call(rbind, lapply(1:nrow(A),FUN=function(i){
      purchased_cp <- paste(testing_coupons[order(score[i,], decreasing = TRUE)][1:10],collapse=" ")
      return(purchased_cp)
    }))
    
    PURCHASED_COUPONS_mt <- PURCHASED_COUPONS[match(map_ready_ground_truth[, 1], 
                                                    users)]
    errm <- mapk(10, map_ready_ground_truth, PURCHASED_COUPONS_mt)
    
    # Get cosine sim predictions comparison
    score2 = as.matrix(A) %*% W %*% t(as.matrix(B))
    #order the list of coupons according to similairties and take only first 10 coupons
    PURCHASED_COUPONS2 <- do.call(rbind, lapply(1:nrow(A),FUN=function(i){
      purchased_cp <- paste(testing_coupons[order(score2[i,], decreasing = TRUE)][1:10],collapse=" ")
      return(purchased_cp)
    }))
    
    PURCHASED_COUPONS_mt2 <- PURCHASED_COUPONS2[match(map_ready_ground_truth[, 1], 
                                                    users)]
    errm2 <- mapk(10, map_ready_ground_truth, PURCHASED_COUPONS_mt2)
    
    if (WK == 1){Glob_D <- diag(D)}
    else{Glob_D <- pmax(Glob_D, diag(D))}
    
    print(paste('On weeks ', WK, 'error is ', errm))
    print(paste('progress: ', WK/length(wk_pairs), 'error is ', errm))
    print(paste('Comparison to original tune is ', errm2))
    # cleaning up
    rm(list = c('A', 'A_samp', 'B', 'D', 
                'PURCHASED_COUPONS', 'PURCHASED_COUPONS_mt', 'R',
                'PURCHASED_COUPONS2', 'PURCHASED_COUPONS_mt2', 
         'score', 'R_NT', 'R_TARGETS', 'W', 'results'))
    gc()
    
  }
  return(Glob_D)
}

tuner()
# 
# score = as.matrix(A) %*% D %*% t(as.matrix(B))
# #order the list of coupons according to similairties and take only first 10 coupons
# uchar$PURCHASED_COUPONS <- do.call(rbind, lapply(1:nrow(uchar),FUN=function(i){
#   purchased_cp <- paste(test$COUPON_ID_hash[order(score[i,], decreasing = TRUE)][1:10],collapse=" ")
#   return(purchased_cp)
# }))
# 
# #make submission
# submission <- merge(ulist, uchar, all.x=TRUE)
# submission <- submission[,c("USER_ID_hash","PURCHASED_COUPONS")]
# write.csv(submission, file="TST_JOE.csv", row.names=FALSE)
