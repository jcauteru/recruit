library(randomForest)
library(cluster)
library(FNN)
exceptions <<- 0 
replacements <- read.csv('cosine_sim.csv', header = T, stringsAsFactors = F)

TrainAndScore <- function(training_data, target, to_be_scored, coupons){
  
#   if(sum(as.numeric(target)) == length(as.numeric(target)) | 
#        sum(as.numeric(target)) == 0 | length(as.numeric(target)) < 10){
#     
#     training_data$ID <- target
#     exceptions <<- exceptions + 1
#     
#     aggregate <- aggregate(.~ID, data = training_data, FUN = mean)
#     result <- get.knnx(aggregate[, -c(1)], to_be_scored, k=1)$nn.dist
#     
#     predicted <- coupons[order(result, decreasing = FALSE)][1:10]
#     returning <- paste(predicted, collapse = ' ')
#     return(returning)
#   }

  # All Purchases are views
  trained_model <- randomForest(training_data, target)
  scored_proba <- predict(trained_model, to_be_scored, type='prob')[, 2]
  
  predicted <- coupons[order(scored_proba, decreasing = TRUE)][1:10]
  returning <- paste(predicted, collapse = ' ')
  return(returning)
  
}

UserByUser <- function(input_user){

  user_coups <- getUserTraining(input_user)
  
  pull_from <- match(user_coups$COUPON_ID_hash,all_training_coupon_ids)
  user_training <- all_training_data[pull_from[!is.na(pull_from)], ]
  TARGET <- as.factor(user_coups$PURCHASE_FLG[!is.na(pull_from)])
  
  if(sum(as.numeric(TARGET)) == length(as.numeric(TARGET)) | 
       sum(as.numeric(TARGET)) == 0 | length(as.numeric(TARGET)) < 20){
    exceptions <<- exceptions + 1
    result <- replacements$PURCHASED_COUPONS[replacements$USER_ID_hash == input_user]
    return(c(as.character(input_user), result))
  }
  
  result <- TrainAndScore(user_training, TARGET, 
                          all_testing_data, all_testing_coupon_ids)
  
  return(c(as.character(input_user), result))
}