setwd('/media/hdd/kaggle/recruit/data')
source('/media/hdd/kaggle/recruit/code/dev/evaluation-mapk.R')


coup_det_train <- read.csv("coupon_detail_train.csv", header = T, stringsAsFactors = F)
coup_visit_train <- read.csv("coupon_visit_train.csv", header = T, stringsAsFactors = F)

# Coupon Information
coup_list_train <- read.csv("coupon_list_train.csv", header = T, stringsAsFactors = F)
coup_list_test <- read.csv("coupon_list_test.csv", header = T, stringsAsFactors = F)
coup_area_test <- read.csv("coupon_area_test.csv", header = T, stringsAsFactors = F)
coup_area_train <- read.csv("coupon_area_train.csv", header = T, stringsAsFactors = F)


## break out:

coup_list_train <- read.csv("coupon_list_train.csv", header = T, stringsAsFactors = T)
coup_list_test <- read.csv("coupon_list_test.csv", header = T, stringsAsFactors = T)

coup_list_train$HOLD <- TRUE
coup_list_test$HOLD <- FALSE

all_data <- rbind(coup_list_train, coup_list_test)
all_data[is.na(all_data)] <- 1

# Remove overhead variables
all_data <- all_data[, -c(match(c('DISPFROM', 'DISPEND', 
                                  'VALIDFROM', 'VALIDEND'), 
                                names(all_data)))]

all_data_original <- all_data

# Hold out id variables
supporting_cols_num <- which(names(all_data) %in% c('COUPON_ID_hash',  'HOLD'))
supporting_cols <- all_data[, supporting_cols_num]

# expand to dummies 
factors <- which(sapply(all_data[, -supporting_cols_num], is.factor)==TRUE)
request_contrasts <- lapply(all_data[,names(factors)], contrasts, contrasts=FALSE)
expanded <- model.matrix(~ -1 + ., all_data[, -supporting_cols_num], contrasts.arg= request_contrasts)
all_data <- cbind(all_data[, supporting_cols_num], expanded)

# Resplit into train and test
train <- all_data[supporting_cols$HOLD == T, ]
test <- all_data[supporting_cols$HOLD == F, ]

# Find coupon distances using gowers distance (http://venus.unive.it/romanaz/modstat_ba/gowdis.pdf)
library(cluster)
library(FastKNN)
library(parallel)
# coupon_gower_distance <- daisy(all_data, "gower")

coupon_gower_distance_DIST <- daisy(all_data_original, "gower")
coupon_gower_distance_DF <- as.matrix(coupon_gower_distance_orig)

quick_sep  <- function(num, data){
  return(data[num, (nrow(coup_list_train) + 1):ncol(coupon_gower_distance_DF)])
} 

test_coupon_pairs <- do.call(rbind, 
                             mclapply(1:nrow(coup_list_train), 
                                      quick_sep, 
                                      coupon_gower_distance_DF, mc.cores=6))

# Let's Check:
coup_list_test[which.min(test_coupon_pairs[2, ]), ]
coup_list_train[2, ]

# Seems to work.

# Now try same appraoch with average user coupons
train_users <- merge(coup_det_train, coup_list_train)
train_users <- train_users[, -c(match(c('DISPFROM', 'DISPEND', 'VALIDFROM', 'VALIDEND', 
                                                                   'ITEM_COUNT', 'I_DATE', 
                                                                   'SMALL_AREA_NAME', 
                                                                   'PURCHASEID_hash'), names(train_users)))
                                ]
coup_list_test$USER_ID_hash <- 'FALSE'
coup_list_test_users <- coup_list_test[, -c(match(c('DISPFROM', 'DISPEND', 'VALIDFROM', 'VALIDEND'),
                                                  names(coup_list_test)))]
coup_list_test_users <- coup_list_test_users[, names(train_users)]

all_data <- rbind(train_users, coup_list_test_users)
all_data[is.na(all_data)] <- 1
# Hold out id variables
supporting_cols_num <- which(names(all_data) %in% c('COUPON_ID_hash',  'USER_ID_hash'))
supporting_cols <- all_data[, supporting_cols_num]

# expand to dummies 
factors <- which(sapply(all_data[, -supporting_cols_num], is.factor)==TRUE)
request_contrasts <- lapply(all_data[,names(factors)], contrasts, contrasts=FALSE)
expanded <- model.matrix(~ -1 + ., all_data[, -supporting_cols_num], contrasts.arg= request_contrasts)
all_data <- cbind(all_data[, supporting_cols_num], expanded)

train_users <- aggregate(supporting_cols$USER_ID_hash[supporting_cols$USER_ID_HASH != 'FALSE'],
                         train[supporting_cols$USER_ID_hash!="FALSE",], 
                         mean)

users <- read.csv("user_list.csv")


# Coupon similiarty:
head(coup_list_train)
head(coup_list_train)
