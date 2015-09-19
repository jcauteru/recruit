setwd('/media/hdd/kaggle/recruit/data')
source('/media/hdd/kaggle/recruit/code/dev/data_process.R')
source('/media/hdd/kaggle/recruit/code/dev/evaluation-mapk.R')

library(doParallel) # Used to compute the user matrix
library(data.table) # Used to Rbind the results 
library(speedglm)
library(Matrix)

DT_ST <- as.Date('2011-07-01', format = '%Y-%m-%d')
DT_ED <- as.Date('2012-06-23', format = '%Y-%m-%d')

weeks <- as.character(c(seq(DT_ST, DT_ED, 6), DT_ED))
total <- length(weeks)
wk_pairs <- 


getData('cos_sim_ser', weeks[total - 2], weeks[total])

source('/media/hdd/kaggle/recruit/code/dev/model_process_cst.R')

# validation_coupon_index
# formatted_validation_set
# val_removed_training_data
# map_ready_ground_truth
# all_training_data
# all_training_coupon_ids
# all_testing_data
# all_testing_coupon_ids

sample_iterations <- 6

feature_weights <- mclapply(1:sample_iterations, cosine_tuner, R_TARGETS, R_NT, mc.cores = 7)
D <- return_weights(feature_weights, paste('V', 1:ncol(A), sep=''))
print(sum(diag(D) != 0))


