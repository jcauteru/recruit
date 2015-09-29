setwd('/media/hdd/kaggle/recruit/data')

tuning_base <- read.csv('RUNNING_BASE.csv', header = T, stringsAsFactors = F)

setwd('/media/hdd/kaggle/recruit/data')
cpdtr <- read.csv("coupon_detail_train.csv")
cpltr <- read.csv("coupon_list_train.csv")
cplte <- read.csv("coupon_list_test.csv")
ulist <- read.csv("user_list.csv")
clusterlookup <- read.csv("cluslookup.csv")
#making of the train set
train <- merge(cpdtr,cpltr)
train <- train[,c("COUPON_ID_hash","USER_ID_hash",
                  "GENRE_NAME","DISCOUNT_PRICE","PRICE_RATE",
                  "USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_WED","USABLE_DATE_THU",
                  "USABLE_DATE_FRI","USABLE_DATE_SAT","USABLE_DATE_SUN","USABLE_DATE_HOLIDAY",
                  "USABLE_DATE_BEFORE_HOLIDAY","large_area_name","ken_name","small_area_name")]
#combine the test set with the train
cplte$USER_ID_hash <- "dummyuser"
cpchar <- cplte[,c("COUPON_ID_hash","USER_ID_hash",
                   "GENRE_NAME","DISCOUNT_PRICE","PRICE_RATE",
                   "USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_WED","USABLE_DATE_THU",
                   "USABLE_DATE_FRI","USABLE_DATE_SAT","USABLE_DATE_SUN","USABLE_DATE_HOLIDAY",
                   "USABLE_DATE_BEFORE_HOLIDAY","large_area_name","ken_name","small_area_name")]

train <- rbind(train,cpchar)
#NA imputation
train[is.na(train)] <- 1
#feature engineering
train$DISCOUNT_PRICE <- 1/log10(train$DISCOUNT_PRICE)
train$PRICE_RATE <- (train$PRICE_RATE*train$PRICE_RATE)/(100*100)
#convert the factors to columns of 0's and 1's
train <- cbind(train[,c(1,2)],model.matrix(~ -1 + .,train[,-c(1,2)],
                                           contrasts.arg=lapply(train[,names(which(sapply(train[,-c(1,2)], is.factor)==TRUE))], contrasts, contrasts=FALSE)))

#separate the test from train
test <- train[train$USER_ID_hash=="dummyuser",]
test <- test[,-2]
train <- train[train$USER_ID_hash!="dummyuser",]

#data frame of user characteristics
uchar <- aggregate(.~USER_ID_hash, data=train[,-1],FUN=mean)
uchar$DISCOUNT_PRICE <- 1
uchar$PRICE_RATE <- 1

#Weight Matrix: GENRE_NAME DISCOUNT_PRICE PRICE_RATE USABLE_DATE_ large_area_name ken_name small_area_name

# Read in the estimations
scores_f1 <- as.matrix(read.csv(
  paste('/media/hdd/kaggle/recruit/data/scores_f1.csv', sep = '')))

scores_f2 <- as.matrix(read.csv(
  paste('/media/hdd/kaggle/recruit/data/scores_f2.csv', sep = '')))

scores_f3 <- as.matrix(read.csv(
  paste('/media/hdd/kaggle/recruit/data/scores_f3.csv', sep = '')))

scores_f4 <- as.matrix(read.csv(
  paste('/media/hdd/kaggle/recruit/data/scores_f4.csv', sep = '')))


needed <- c('f2','f4')
clusterlookup <- read.csv('cluslookup.csv', header = T, stringsAsFactors = F)
tuning_base <- read.csv('RUNNING_BASE.csv', header = T, stringsAsFactors = F)

tuning_base1 <- tuning_base

for (n in needed){
  print(n)
  W <- as.matrix(read.csv(
    paste('/media/hdd/kaggle/recruit/data/scores_', n, '.csv', sep = '')))
  score = as.matrix(uchar[,2:ncol(uchar)]) %*% W %*% t(as.matrix(test[,2:ncol(test)]))
  PURCHASED_COUPONS <- do.call(rbind, lapply(1:nrow(uchar),FUN=function(i){
    purchased_cp <- paste(test$COUPON_ID_hash[order(score[i,], decreasing = TRUE)][1:10],collapse=" ")
    return(purchased_cp)
  }))
  grab_id <- as.character(clusterlookup[clusterlookup$cluster == n,"USER_ID_hash"])
  tuning_base[tuning_base$USER_ID_HASH %in% grab_id, 1] <- PURCHASED_COUPONS[tuning_base$USER_ID_HASH %in% grab_id]
}


tuning_base <- tuning_base[, c('USER_ID_HASH', 'PURCHASE_COUPONS')]
names(tuning_base) <- c('USER_ID_HASH', 'PURCHASED_COUPONS')
write.csv(tuning_base, file='enet_test_1.csv', row.names = F)
