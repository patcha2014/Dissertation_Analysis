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

# 2005 -----------------------------
# Variables we need is in s2a file

y05s2a <- csv.get(file="w1s2_a.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)
colnames(y07s2a)
ses <- y05s2a[c(3,9,84:86)] # select only hh_id (hid), member no (pno). and recent occupation list (f4.1-3)
colnames(ses) <- c("h_id","m_id","occu1_05","occu2_05","occu3_05")
rm(y05s2a)

# 2006 -----------------------------

y06s2 <- csv.get(file="w2s2.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)
colnames(y06s2)
y06s2 <- y06s2[c(5,12,73:75)]
colnames(y06s2) <- c("h_id","m_id","occu1_06","occu2_06","occu3_06")
ses <- cbind(ses,y06s2)


ses05 <- y07s1[,(1:10,)]
nametemp <- colnames(ses05)
coltemp <- paste(nametemp)