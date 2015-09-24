#setwd('/media/hdd/kaggle/recruit/data')
setwd('~/JOE/data')

getMe <- function(){

	train <- coup_list_train
	train$USER_ID_hash <- "realuser"
	train <- train[,c("COUPON_ID_hash","USER_ID_hash",
	                  "GENRE_NAME","DISCOUNT_PRICE","PRICE_RATE",
	                  "USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_WED","USABLE_DATE_THU",
	                  "USABLE_DATE_FRI","USABLE_DATE_SAT","USABLE_DATE_SUN","USABLE_DATE_HOLIDAY",
	                  "USABLE_DATE_BEFORE_HOLIDAY","large_area_name","ken_name","small_area_name")]
	#combine the test set with the train
	coup_list_test$USER_ID_hash <- "dummyuser"
	cpchar <- coup_list_test[,c("COUPON_ID_hash","USER_ID_hash",
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

	train$DISCOUNT_PRICE <- 1
	train$PRICE_RATE <- 1


	## SUBSET TO FEMALES HERE AND RETURN...CAN ALSO DO ABOVE ##
	subsetids <<- clusterlookup[clusterlookup$cluster == "f1","USER_ID_hash"]

	all_training_data <<- train[train$USER_ID_hash %in% subsetids, -c(1, 2)]
	all_training_coupon_ids <<- train[train$USER_ID_hash %in% subsetids, c(1,2)]
	  
	all_testing_data <<- test[test$USER_ID_hash %in% subsetids, -c(1, 2)]
	all_testing_coupon_ids <<- test[test$USER_ID_hash %in% subsetids, c(1,2)]

}

getMe()
