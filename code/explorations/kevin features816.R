#setwd('/media/hdd/kaggle/recruit/data')
#source('/media/hdd/kaggle/recruit/code/dev/evaluation-mapk.R')
library(Matrix)
setwd('Kaggle/Ponpare/')

#read in all the input data
cpdtr <- read.csv("coupon_detail_train.csv")
cpdtr$PurchaseDate <- strptime(cpdtr$I_DATE, format='%Y-%m-%d %H:%M:%S')
cpdtr <- cpdtr[,-2]
cpltr <- read.csv("coupon_list_train.csv")
cplte <- read.csv("coupon_list_test.csv")
ulist <- read.csv("user_list.csv")

#Added large area name and capsule text, binned price rate and discount price
train <- merge(cpdtr,cpltr)

train <- train[,c("COUPON_ID_hash","USER_ID_hash",
                  "GENRE_NAME","DISCOUNT_PRICE","PRICE_RATE",
                  "USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_WED","USABLE_DATE_THU",
                  "USABLE_DATE_FRI","USABLE_DATE_SAT","USABLE_DATE_SUN","USABLE_DATE_HOLIDAY",
                  "USABLE_DATE_BEFORE_HOLIDAY","large_area_name","ken_name","small_area_name", 
                  "CAPSULE_TEXT","DISPFROM", "DISPEND", "DISPPERIOD","VALIDFROM", "VALIDEND", "VALIDPERIOD", "PurchaseDate")]
#features to add: days since user registered, days since last purchase, average days bw purchases,
#Create variables related to dates: a) purchased in discount window? b) days between purchase date and 
#coupon expiration date, c) days bw purchase date and coupon validity start date


#combine the test set with the train
cplte$USER_ID_hash <- "dummyuser"
cplte$PurchaseDate <- "2012-06-26 00:00:00"
cpchar <- cplte[,c("COUPON_ID_hash","USER_ID_hash",
                   "GENRE_NAME","DISCOUNT_PRICE","PRICE_RATE",
                   "USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_WED","USABLE_DATE_THU",
                   "USABLE_DATE_FRI","USABLE_DATE_SAT","USABLE_DATE_SUN","USABLE_DATE_HOLIDAY",
                   "USABLE_DATE_BEFORE_HOLIDAY","large_area_name","ken_name","small_area_name", 
                   "CAPSULE_TEXT","DISPFROM", "DISPEND", "DISPPERIOD","VALIDFROM", "VALIDEND", 
                   "VALIDPERIOD", "PurchaseDate")]

#NEED TO PREDICT AVERAGE USER FEATURES FOR cpchar BASED ON OTHER FEATURES
train <- rbind(train,cpchar)
train <- merge(train, ulist)

#Convert dates
train$DISPFROM <- strptime(train$DISPFROM, format='%Y-%m-%d %H:%M:%S')
train$DISPEND <- strptime(train$DISPEND, format='%Y-%m-%d %H:%M:%S')
train$VALIDFROM <- strptime(train$VALIDFROM, format='%Y-%m-%d')
train$VALIDEND <- strptime(train$VALIDEND, format='%Y-%m-%d')

#DEAL WITH NA'S: VALIDFROM (Min validfrom), VALIDEND(max validfrom), VALIDDAYS(1's), rm withdrawdate
#train$VALIDFROM[is.na(train$VALIDFROM)] <- min(train$VALIDFROM,na.rm = TRUE)
#train$VALIDEND[is.na(train$VALIDEND)] <- max(train$VALIDEND, na.rm = TRUE)
for (i in 6:14){
  train[which(is.na(train[,i])),i] <- 1
}
train <- train[,-29] #withdraw date
train$VALIDPERIOD <- as.numeric(difftime(train$VALIDEND, train$VALIDFROM, units ="days"))
train$DISPENDtoVALIDFROM <- as.numeric(difftime(train$VALIDFROM, train$DISPEND, units ="days")) #sale end to valid start
train$DISPFROMtoVALIDFROM <- as.numeric(difftime(train$VALIDFROM, train$DISPFROM, units ="days")) #sale start to valid start
train$DISPENDtoVALIDEND <- as.numeric(difftime(train$VALIDEND, train$DISPEND, units ="days")) #sale end to valid end
train$DISPFROMtoVALIDEND <- as.numeric(difftime(train$VALIDEND, train$DISPFROM, units ="days")) #sale start to valid end

#times from purchase date
train$SLstP <- as.numeric(difftime(train$PurchaseDate, train$DISPFROM, units ="days")) #sale start to purchase
train$VALstP <- as.numeric(difftime(train$PurchaseDate, train$VALIDFROM, units ="days")) #valid start to purchase
train$VALendP <- as.numeric(difftime(train$PurchaseDate, train$VALIDEND, units ="days")) #valid end to purchase
train$SLendP <- as.numeric(difftime(train$PurchaseDate, train$DISPEND, units ="days")) #sale end to purchase

#TIMES FROM REG_DATE
train$REGtoP <- as.numeric(difftime(train$REG_DATE, train$PurchaseDate, units ="days")) 
train$REGtoVF <- as.numeric(difftime(train$REG_DATE, train$VALIDFROM, units ="days")) 
train$REGtoVE <- as.numeric(difftime(train$REG_DATE, train$VALIDEND, units ="days")) 
train$REGtoDE <- as.numeric(difftime(train$REG_DATE, train$DISPEND, units ="days")) 
train$REGtoDF <- as.numeric(difftime(train$REG_DATE, train$DISPFROM, units ="days"))


#Price Rate is discount price as % of list price
train$PriceRateBin <- as.factor(ifelse(train$PRICE_RATE <= 50, "<50",
                                    ifelse(train$PRICE_RATE <= 60, "50<60",
                                          ifelse(train$PRICE_RATE <= 70, "60<70",
                                                 ifelse(train$PRICE_RATE <= 80, "70<80",
                                                        ifelse(train$PRICE_RATE <= 90, "80<90","90<100"))))))
#Discount Rate Binning:		0-1k, 1k-2k, 2k-3k, 4k, 5k, 6k, 100k
train$DiscountPriceBin <- as.factor(ifelse(train$DISCOUNT_PRICE <= 1000, "<1k",
                                       ifelse(train$DISCOUNT_PRICE <= 2000, "1k<2k",
                                            ifelse(train$DISCOUNT_PRICE <= 3000, "2k<3k",
                                                   ifelse(train$DISCOUNT_PRICE <= 4000, "3k<4k",
                                                          ifelse(train$DISCOUNT_PRICE <= 5000, "4k<5k",
                                                                 ifelse(train$DISCOUNT_PRICE <= 6000, "5k<6k", ">6k")))))))
#Convert Date Vars to Bins/factors
train$DISPENDtoVALIDFROM <- cut(train$DISPENDtoVALIDFROM, breaks = c(-Inf,-5,-1,-0.5,0,.5,1,5,Inf))
train$DISPFROMtoVALIDFROM <- cut(train$DISPFROMtoVALIDFROM, breaks = c(-Inf,0,1,5,10,Inf))
train$DISPENDtoVALIDEND <- cut(train$DISPENDtoVALIDEND, breaks = c(-Inf,50,100,150,Inf))
train$DISPFROMtoVALIDEND <- cut(train$DISPFROMtoVALIDEND, breaks = c(-Inf,60,120,160,Inf))
train$SLstP <- cut(train$SLstP, breaks = c(0,1,2,3,Inf))
train$VALstP <- cut(train$VALstP, breaks = c(0,1,2,3,Inf))              

train$VALendP <- cut(train$VALendP, breaks = c(-Inf,-175,-125,-100,-70,Inf))                        
train$SLendP <- cut(train$SLendP, breaks = c(-Inf,-3,-2,-1,0,Inf))                        
train$REGtoP <- cut(train$REGtoP, breaks = c(-Inf,-300,-200,-100,0))
train$REGtoVF <- cut(train$REGtoVF, breaks = c(-Inf,-300,-200,-100,0))
train$REGtoVE <- cut(train$REGtoVE, breaks = c(-Inf,-400,-300,-200,-100,Inf))
train$REGtoDE <- cut(train$REGtoDE, breaks = c(-Inf,-300,-200,-100,0))            
train$REGtoDF <- cut(train$REGtoDF, breaks = c(-Inf,-300,-200,-100,Inf))           

#DISPFROM, DISPEND, VALIDFROM, VALIDEND, PurchaseDate, Reg_Date
#convert to factors??



#unique area ken, 9 large area names, 55 kens
#22783 unique users, 130272 unique user - ken combos
#length(unique(cbind(train$USER_ID_hash, train$small_area_name)))
#length(unique(train$small_area_name))

train <- cbind(train[,c(1,2,3,15)],
               model.matrix(~ -1 + .,train[,-c(1,2)],
                            contrasts.arg=lapply(train[,names(which(sapply(train[,-c(1,2)], is.factor)==TRUE))], 
                                                 contrasts, contrasts=FALSE)))

test <- train[train$USER_ID_hash=="dummyuser",]
test <- test[,-2]
train <- train[train$USER_ID_hash!="dummyuser",]

uchar <- aggregate(.~USER_ID_hash+GENRE_NAME+large_area_name, data=train,FUN=mean)[,-4]
ucharnames <- as.data.frame(names(uchar))
uchar$DISCOUNT_PRICE <- 1
uchar$PRICE_RATE <- 1

# NEED TO LEARN THESE WEIGHTINGS
W <- as.matrix(Diagonal(x=
                          c(rep(4,13),  # Genre	13
                            rep(1,2),     # Discount	1, PriceRate	1,
                            rep(0.2,7),   # UsableDays	7,
                            rep(0.2,2),     # UsableHol	2,
                            rep(0,9),     #largearea	9, removed
                            rep(1,47),    # ken	47
                            rep(1,55),    # smallarea	55, 
                            rep(4,25),    #capsule	25, 
                            rep(4,6),     #pricerate	6, 
                            rep(4,7))))   #discprice	7

# uchar is a matrix of features each user values, W is weight matrix of those features, 
# test are features of test coupons
# area_genre_combos <- #9 areas by 13 genres = 117 combos
# for coupons matching area/genre, multiply user area/genre by that, if 

score = as.matrix(uchar[,2:ncol(uchar)]) %*% W %*% t(as.matrix(test[,2:ncol(test)]))



###########STOPPED HERE, FIGURING OUT WEIGHTS ###############


#order the list of coupons according to similarities and take only first 10 coupons
uchar$PURCHASED_COUPONS <- do.call(rbind, lapply(1:nrow(uchar),FUN=function(i){
  purchased_cp <- paste(test$COUPON_ID_hash[order(score[i,], decreasing = TRUE)][1:10],collapse=" ")
  return(purchased_cp)
}))



# Predict on last 7 days - 2012-06-16 to 2012-06-23
train_details <- cpdtr[which(cpdtr$DATEposix < "2012-06-16 00:00:00"),] #18893
test_details <- cpdtr[which(cpdtr$DATEposix >= "2012-06-16 00:00:00"),] #589 coupon IDs

validation_coupons <- unique(test_details$COUPON_ID_hash) #remove coupons that are in train_details$COUPON_ID_hash
validation_coupons <- validation_coupons[! validation_coupons %in% unique(train_details$COUPON_ID_hash)] 



#make submission
submission <- merge(ulist, uchar, all.x=TRUE)
submission <- submission[,c("USER_ID_hash","PURCHASED_COUPONS")]
write.csv(submission, file="~/cosine_sim_subtestkevin1.csv", row.names=FALSE)
