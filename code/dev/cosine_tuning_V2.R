setwd('/media/hdd/kaggle/recruit/data')
source('/media/hdd/kaggle/recruit/code/dev/data_process.R')
source('/media/hdd/kaggle/recruit/code/dev/evaluation-mapk.R')
#setwd('/home/ubuntu/JOE/data')
#source('/home/ubuntu/JOE/recruit/code/dev/data_process.R')
#source('/home/ubuntu/JOE/recruit/code/dev/evaluation-mapk.R')

clusterlookup <- read.csv('cluslookup.csv', header = T, stringsAsFactors = F)
clusterlookup <- clusterlookup[, -1]

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

desired_cluster <- 'f3'
subset <- as.character(clusterlookup[clusterlookup$cluster == desired_cluster,"USER_ID_hash"])

Glob_D <- list()

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
    try(feature_weights <- mclapply(1:sample_iterations, cosine_tuner3, R_TARGETS, R_NT, mc.cores = 7))
    try(D <- return_weights(feature_weights, paste('V', 1:ncol(A), sep='')))
    
    Glob_D <- stack_diag(D, WK, Glob_D)
    
    print(paste('On weeks ', WK))
    print(paste('progress: ', WK/length(wk_pairs)))

    # cleaning up
    rm(list = c('A', 'A_samp', 'B', 'D', 'R', 'A_samp_users',
         'R_NT', 'R_TARGETS', 'W', 'results', 
         'validation_coupon_index', 
         'formatted_validation_set', 'val_removed_training_data', 
         'map_ready_ground_truth', 'coup_det_train_sub', 
         'testing_coupons', 'testing_purchases', 'training_users_det', 
         'training_coupons', 'train', 'dt_tr', 'users', 'purchase_users', 
         'non_purchase_users', 'sample_indecies', 
         'feature_weights', 'my_clus'))

    gc()
    
  }
  
  return(do.call(rbind, Glob_D))  

}

ESTIMATION <- tuner()

weights <- colMeans(ESTIMATION)
D <- as.matrix(Diagonal(length(weights), weights))
write.csv(D, 
          file=paste('/media/hdd/kaggle/recruit/data/scores_', desired_cluster, '.csv', sep = ''), 
          row.names = F)
