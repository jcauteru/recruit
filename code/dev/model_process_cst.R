
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
sample_indecies <-  c(match(sample(purchase_users, length(purchase_users)), users),
                      match(sample(non_purchase_users, length(purchase_users)), users))
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

stopCluster(my_clus)
R <- as.data.frame(rbindlist(results))

# Random Sample Purchases of coupons and non purchses and train weights
# Then Iterate this process
R_TARGETS <- R[R$TARGET == TRUE, ]
R_NT <- R[R$TARGET == FALSE, ]

cosine_tuner <- function(i, TARS, NO_TARS){
  
  set.seed(round((i*runif(1, 1, 30000))/runif(1, 20, 3854))) 
  for_tuning <- rbind(TARS, NO_TARS[sample(1:nrow(NO_TARS), nrow(R_TARGETS)), ])[, -136]
  
  # Hold Var Names:
  v_name_hold <- names(for_tuning)
  new_names <- paste('V', 1:ncol(for_tuning), sep='')
  names(for_tuning) <- new_names
  # this is broken...
  rm_cols <- which(colSums(for_tuning) == 0)
  for_tuning <- for_tuning[, -c(rm_cols)]
  indep <-paste(names(for_tuning)[-ncol(for_tuning)], collapse='+')
  fo <- as.formula(paste(names(for_tuning)[ncol(for_tuning)], "~-1+", indep, sep=''))
  print(fo)
  regres <- bigglm(fo, for_tuning, family=binomial(), maxit=10)
  results <- coef(summary(regres))
  vars <- row.names(results)
  weight <- results[, 1]
  weight_pv <- as.numeric(results[, 4])
  final <- data.frame(VAR=vars[weight_pv <= .1], WT=weight[weight_pv <= .1])
  return(final)
}


cosine_tuner2 <- function(i, TARS, NO_TARS){
  
  set.seed(round((i*runif(1, 1, 30000))/runif(1, 20, 3854))) 
  for_tuning <- rbind(TARS, NO_TARS[sample(1:nrow(NO_TARS), nrow(R_TARGETS)), ])[, -136]
  
  # Hold Var Names:
  v_name_hold <- names(for_tuning)
  new_names <- paste('V', 1:ncol(for_tuning), sep='')
  names(for_tuning) <- new_names
  # this is broken...
  rm_cols <- which(colSums(for_tuning) == 0)
  for_tuning <- for_tuning[, -c(rm_cols)]
  indep <-paste(names(for_tuning)[-ncol(for_tuning)], collapse='+')
  fo <- as.formula(paste(names(for_tuning)[ncol(for_tuning)], "~-1+", indep, sep=''))
  print(fo)
  regres <- glm(fo, for_tuning, family='binomial')
  results <- coef(summary(regres))
  vars <- row.names(results)
  weight <- results[, 1]
  weight_pv <- as.numeric(results[, 4])
  final <- data.frame(VAR=vars[weight_pv <= .1], WT=weight[weight_pv <= .1])
  return(final)
}

return_weights <- function(weight_list, name_set){
  weight_list_mod <- lapply(weight_list, function(x){if(nrow(x) > 0){return(x)}})
  mean_aggregate <- as.data.frame(aggregate(.~VAR, data=do.call(rbind, weight_list_mod), FUN = max))
  mean_aggregate$VAR <- as.character(mean_aggregate$VAR)
  mean_aggregate$VAR <- gsub('`', '',mean_aggregate$VAR)
  fill <- rep(0, length(name_set))
  fill[match(mean_aggregate$VAR, name_set)] <- mean_aggregate$WT
  return(as.matrix(Diagonal(length(fill), fill)))
}
