# Less-skilled immigration and natives' outcomes
# Spring 2016
# Patcha Chaikitmongkol

rm(list=ls()) # clear workspace

# SES Panel 

setwd("/Users/Mint/Dropbox/Dissertation_Data/SESpanel")

library(Hmisc)

# f4 = occupation
# i4 = migration type (1=within prov,2=within reg,3=across region,4=from outside Thailand,9=not reported,blank=not applicable)
# i5 = previous province 
# i6 = main reason for moving (01=job search,02=job change,05=school)

#y07s1 <- csv.get(file="w1s1.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)
y07s2a <- csv.get(file="w1s2_a.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)
#y07s2b <- csv.get(file="w1s2_b.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)
#colnames(y07s1); colnames(y07s2b)
colnames(y07s2a)
y07 <- y07s2a[c(3,84:86)]
colnames(y07) <- c("hid","occu1_0")
# Want to cbind by c("w","serial.no","h","check.no","hid","reg","area","psu.no","hh.no","members15")

# f4 = occupation (in 3 recent periods)
table(y07s2a$f4.1); table(y07s2a$f4.2); table(y07s2a$f4.3)




y08s1 <- csv.get(file="w2s1.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)
y08s2 <- csv.get(file="w2s2.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)
colnames(y08s1); colnames(y08s2)
library(plyr)
y08 <- rbind.fill(y08s1,y08s2)

ses05 <- y07s1[,(1:10,)]
nametemp <- colnames(ses05)
coltemp <- paste(nametemp)