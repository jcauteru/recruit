# Nested RF #
setwd('/media/hdd/kaggle/recruit/data')
source('/media/hdd/kaggle/recruit/code/dev/data_process.R')
source('/media/hdd/kaggle/recruit/code/dev/evaluation-mapk.R')

library(parallel)



getData()

no_data_users <- read.csv('no_data_users.csv', header = T, stringsAsFactors = F)
## Grab User Visit Data
all_users <- setdiff(user_list$USER_ID_hash, no_data_users$USER_ID_hash)

iter_over <- c(seq(0, length(all_users), 100), length(all_users))
results <- list()
for (i in 1:(length(iter_over)-1)){
  st <- iter_over[i]+1
  ed <- iter_over[i+1]
  print(st)
  print(ed)
  print('START')
  attempt <- mclapply(all_users[st:ed], UserByUser, mc.cores = 7)
  ds_res <- do.call(rbind.data.frame, attempt)
  names(ds_res) <- c("USER_ID_hash","PURCHASED_COUPONS")
  results[[i]] <- ds_res
  print(exceptions)
}

final <- do.call(rbind.data.frame, results)

filler <- names(table(final$PURCHASED_COUPONS))[
  which.max(table(final$PURCHASED_COUPONS))]

no_data_users$PURCHASED_COUPONS <- filler

submission <- rbind(no_data_users, final)

write.csv(submission, file="RFattempt2.csv", row.names=FALSE)



## Test the Data Processing ##

