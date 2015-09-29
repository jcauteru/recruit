setwd('/media/hdd/kaggle/recruit/data/')

f1 <- diag(as.matrix(read.csv('scores_f1.csv')))
f2 <- diag(as.matrix(read.csv('scores_f2.csv')))
f3 <- diag(as.matrix(read.csv('scores_f3.csv')))
f4 <- diag(as.matrix(read.csv('scores_f4.csv')))

m1 <- diag(as.matrix(read.csv('scores_m1.csv')))
m2 <- diag(as.matrix(read.csv('scores_m2.csv')))
m3 <- diag(as.matrix(read.csv('scores_m3.csv')))

things <- names(uchar)[-1]

block <- data.frame(f1=f1, f2=f2, f3=f3, f4=f4, m1=m1, m2=m2, m3=m3)
heatmap(t(as.matrix(block[1:13, ])),dendrogram='none', Rowv=FALSE, Colv=FALSE,trace='none')

block[6:7, ]
male_coup_st <- rep(1.96,13)
female_coup_st <- rep(1.75,13)

#FM over MALE
c(3,8) # shocker females like nail salons and massage

# M1 and F1
c(13,2,14) #like Home Delivery, Food, hotels
c(4, 10) #No like lessons relax

#F3 Really like 9, gift cards

# NEXT BLOCK

heatmap(t(as.matrix(block[1:13, -c(1,5)])),dendrogram='none', Rowv=FALSE, Colv=FALSE,trace='none')


