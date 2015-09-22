setwd('/media/hdd/kaggle/recruit/data')


coup_det_train <- read.csv("coupon_detail_train.csv", header = T, stringsAsFactors = F)
coup_visit_train <- read.csv("coupon_visit_train.csv", header = T, stringsAsFactors = F)

# Coupon Information
coup_list_train <- read.csv("coupon_list_train.csv", header = T, stringsAsFactors = F)
coup_list_test <- read.csv("coupon_list_test.csv", header = T, stringsAsFactors = F)
coup_area_test <- read.csv("coupon_area_test.csv", header = T, stringsAsFactors = F)
coup_area_train <- read.csv("coupon_area_train.csv", header = T, stringsAsFactors = F)
user_list <- read.csv("user_list.csv")

length(unique(c(coup_visit_train$USER_ID_hash, coup_det_train$USER_ID_hash)))
no_data_users <- user_list$USER_ID_hash[!(user_list$USER_ID_hash %in% 
                                            unique(c(coup_visit_train$USER_ID_hash, 
                                                     coup_det_train$USER_ID_hash)))]

write.csv(data.frame(USER_ID_hash=no_data_users), file = 'no_data_users.csv', row.names = F)

length(unique(coup_det_train$USER_ID_hash))
dim(coup_det_train)
max(table(coup_det_train$USER_ID_hash))
hist(table(coup_det_train$USER_ID_hash), breaks = 500)

#mvp = names(which.max((table(coup_det_train$USER_ID_hash))))
mvp = user_list$USER_ID_hash[963]
user_info <- user_list[user_list$USER_ID_hash == mvp, ]
mvp_bought = names(table(coup_det_train$COUPON_ID_hash[coup_det_train$USER_ID_hash == mvp]))
MVP_B_NAME <- names(table(coup_det_train$COUPON_ID_hash[coup_det_train$USER_ID_hash == mvp]))
MVP_BUYS <- coup_list_train[coup_list_train$COUPON_ID_hash %in% MVP_B_NAME, ]

MVP_BROW <- coup_visit_train[coup_visit_train$USER_ID_hash %in% mvp, ]
dim(MVP_BROW)
MVP_BROW <- MVP_BROW[order(MVP_BROW$I_DATE), ]
MVP_BROW

