setwd('/media/hdd/kaggle/recruit/data')

### All users ###
users <- read.csv('user_list.csv', header = T, stringsAsFactors = F)

##### High level coupon list #####
coupon_list_train <- read.csv('coupon_list_train.csv', header = T, stringsAsFactors = F)

## How many types?
length(unique(coupon_list_train$CAPSULE_TEXT))
## Only 25 different capsule texts

## Same for Genre names??
length(unique(coupon_list_train$GENRE_NAME))
## Across 13 different genre names:

## Genere Names by Capsule Text ##


##### Japan Area Names #####


##### Japan Area Names #####

coupon_detail_train <- read.csv('coupon_detail_train.csv', header = T, stringsAsFactors = F)

### The sequence of browsing information ###
coupon_visit_train <- read.csv('coupon_visit_train.csv', header = T, stringsAsFactors = F)

sum(unique(coupon_visit_train$REFERRER_hash) %in% users$USER_ID_hash)



