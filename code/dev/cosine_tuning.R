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
getData('cos_sim_ser', weeks[total - 2], weeks[total])

# validation_coupon_index
# formatted_validation_set
# val_removed_training_data
# map_ready_ground_truth
# all_training_data
# all_training_coupon_ids
# all_testing_data
# all_testing_coupon_ids

# Remove user Records as well:

### Internal Test Data Processing  ###
testing_coupons <- coup_list_train$COUPON_ID_hash[validation_coupon_index]
testing_purchases <- coup_det_train[coup_det_train$COUPON_ID_has %in% testing_coupons, 
                                    c(5, 6)]

### Training Data rocessing ###
training_users_det <- coup_det_train[!(coup_det_train$COUPON_ID_hash %in% 
                                         testing_coupons),
                                     c('USER_ID_hash', 'COUPON_ID_hash')]

training_coupons <- coup_list_train$COUPON_ID_hash[setdiff(1:nrow(coup_list_train),
                                                           validation_coupon_index)]
val_removed_training_data$COUPON_ID_hash <- training_coupons
train <- merge(training_users_det, val_removed_training_data)

### Tuning Data Processing ###
dt_tr <- data.table(train[, -1])
A <- as.data.frame(dt_tr[, lapply(.SD,mean), by=USER_ID_hash])
users <- A$USER_ID_hash
A <- A[, -c(1)]
B <- formatted_validation_set

R_W_PROD <- function(i, DF1, DF2, left_users, rt_ids, tar_check){
  user_purch <- tar_check[tar_check$USER_ID_hash == left_users[i], c('COUPON_ID_hash')]
  right_hand_matrix <- as.matrix(DF2)
  hld_row <- as.numeric(DF1[i, ])
  lldf <- as.data.frame(Reduce('*', list(hld_row, right_hand_matrix)))
  lldf$USER_ID_hash <- left_users[i]
  lldf$TARGET <- rt_ids %in% user_purch
  return(lldf)
}

# randomly select a set of users:
purchase_users <- unique(testing_purchases$USER_ID_hash)
non_purchase_users <- setdiff(user_list$USER_ID_hash, purchase_users)

# Ideally we'd do this process for everyone
sample_indecies <-  c(match(sample(purchase_users, 1000), users),
                      match(sample(non_purchase_users, 1000), users))
sample_indecies <- sample_indecies[!is.na(sample_indecies)]

A_samp <- A[sample_indecies, ]
A_samp_users <- users[sample_indecies]

detectCores()
my_clus <- makeCluster(7)
registerDoParallel(my_clus)
getDoParWorkers()

results <- parLapply(my_clus, 1:nrow(A_samp), 
                     R_W_PROD, A_samp, 
                     B, A_samp_users,
                     testing_coupons, testing_purchases)

R <- as.data.frame(rbindlist(results))

# Random Sample Purchases of coupons and non purchses and train weights
# Then Iterate this process
R_TARGETS <- R[R$TARGET == TRUE, ]
R_NT <- R[R$TARGET == FALSE, ]

cosine_tuner <- function(i, TARS, NO_TARS){
  
  set.seed(round((i*runif(1, 1, 30000))/runif(1, 20, 3854))) 
  for_tuning <- rbind(TARS, NO_TARS[sample(1:nrow(NO_TARS), nrow(R_TARGETS)), ])
  
  # Hold Var Names:
  v_name_hold <- names(for_tuning)
  new_names <- paste('V', 1:ncol(for_tuning), sep='')
  names(for_tuning) <- new_names
  # this is broken...
  rm_cols <- which(colSums(for_tuning) == 0)
  print(rm_cols)
  for_tuning <- for_tuning[, -c(rm_cols, 136)]
  indep <-paste(names(for_tuning)[-ncol(for_tuning)], collapse='+')
  fo <- as.formula(paste(names[for_tuning][ncol(for_tuning)], "~-1+", indep, sep=''))
  print()
  regres <- speedglm(fo, for_tuning, family=binomial())
  results <- coef(summary(regres))
  vars <- row.names(results)
  weight <- results[, 1]
  weight_pv <- results[, 4]
  final <- data.frame(VAR=vars[weight_pv <= .1], WT=weight[weight_pv <= .1])
  return(final)
}

return_weights <- function(weight_list, full_data){
  mean_aggregate <- as.data.frame(aggregate(.~VAR, data=do.call(rbind, weight_list), FUN = mean))
  mean_aggregate$VAR <- as.character(mean_aggregate$VAR)
  mean_aggregate$VAR <- gsub('`', '',mean_aggregate$VAR)
  fill <- rep(0, length(names(full_data)))
  fill[match(mean_aggregate$VAR, names(full_data))] <- mean_aggregate$WT
  return(as.matrix(Diagonal(fill)))
}

sample_iterations <- 2
feature_weights <- mclapply(1:sample_iterations, cosine_tuner, R_TARGETS, R_NT, mc.cores = 7)
D <- return_weights(feature_weights, A)
