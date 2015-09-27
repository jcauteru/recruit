#read in all the input data
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
require(Matrix)
subset <- as.character(clusterlookup[clusterlookup$cluster == "f1","USER_ID_hash"])
fm_weight <- c(4.916639, 8.294844, 4.513375, 3.125729, 1.293260,
               4.699466, 3.613882, 2.961515, 2.087033, 1.669827,
               2.025049, 7.193985, 4.714012, 2.587442, 2.351715,
               1.139181, 6.860886, 7.158422, 1.777239, 2.109310,
               3.714804, 9.457559, 8.828655, 3.960315, 5.638808,
               1.654770, 1.823932, 3.613881, 7.098902, 2.109196,
               5.574781, 1.246123, 1.623433, 3.709543, 9.978798,
               2.421991, 5.444216, 2.580434, 3.784223, 1.942232,
               6.901544, 2.409368, 1.992654, 7.774864, 2.268319,
               1.062831, 4.924699, 5.418538, 2.096808, 1.496572,
               2.811653, 1.075118, 1.491463, 6.772752, 7.294134,
               9.521104, 9.115043, 2.661377, 2.311845, 3.867676,
               3.187414, 1.710589, 2.723237, 2.333870, 2.831646,
               2.481277, 5.708931, 3.290516, 5.326100, 1.863626,
               1.410874, 4.065936, 8.023377, 6.296811, 5.301105,
               4.191555, 6.247861, 5.292175, 1.310847, 2.218655,
               3.952225, 2.147237, 6.948549, 2.994433, 7.554797,
               9.334760, 6.076941, 1.037749, 1.549287, 5.341690,
               4.426570, 2.630827, 8.327627, 1.613179, 8.661551,
               2.796130, 1.588488, 5.371209, 4.191819, 3.412729,
               2.035386, 3.920545, 3.613882, 6.441598, 1.632873,
               5.419468, 1.775891, 1.053049, 3.613882, 6.183408,
               2.293478, 5.167606, 1.678541, 1.725432, 1.992654,
               1.358144, 3.067086, 7.443138, 3.470509, 5.341659,
               3.774622, 2.585199, 2.619710, 5.229878, 1.134644,
               1.555832, 3.613880, 8.975137, 1.188095, 6.845343,
               2.182341, 3.938949, 6.037677, 1.335342, 7.399754)

Wfm <- as.matrix(Diagonal(length(fm_weight), fm_weight))#calculation of cosine similairties of users and coupons
score = as.matrix(uchar[uchar$USER_ID_hash %in% subset,2:ncol(uchar)]) %*% Wfm %*% t(as.matrix(test[,2:ncol(test)]))
#order the list of coupons according to similairties and take only first 10 coupons
PURCHASED_COUPONS_fm <- do.call(rbind, lapply(1:nrow(uchar[uchar$USER_ID_hash %in% subset,]),FUN=function(i){
  purchased_cp <- paste(test$COUPON_ID_hash[order(score[i,], decreasing = TRUE)][1:10],collapse=" ")
  return(purchased_cp)
}))

data_1 <- data.frame(USER_ID_hash=uchar[uchar$USER_ID_hash %in% subset,1], 
                     PURCHASED_COUPONS=PURCHASED_COUPONS_fm)


W <- as.matrix(Diagonal(x=c(rep(2.05,13), rep(2,1), rep(-0.13,1), rep(0,9), rep(0.5,9), rep(1.01,47), rep(4.75,55))))#order the list of coupons according to similairties and take only first 10 coupons
score = as.matrix(uchar[!(uchar$USER_ID_hash %in% subset),2:ncol(uchar)]) %*% W %*% t(as.matrix(test[,2:ncol(test)]))
PURCHASED_COUPONS_m <- do.call(rbind, lapply(1:nrow(uchar[!(uchar$USER_ID_hash %in% subset),]),FUN=function(i){
  purchased_cp <- paste(test$COUPON_ID_hash[order(score[i,], decreasing = TRUE)][1:10],collapse=" ")
  return(purchased_cp)
}))

data_2 <- data.frame(USER_ID_hash=uchar[!(uchar$USER_ID_hash %in% subset),1], 
                     PURCHASED_COUPONS=PURCHASED_COUPONS_m)

uchar <- rbind(data_1, data_2)
#make submission
submission <- merge(ulist, uchar, all.x=TRUE)
submission <- submission[,c("USER_ID_hash","PURCHASED_COUPONS")]
write.csv(submission, file="TUNING_MF_TST_1.csv", row.names=FALSE)
