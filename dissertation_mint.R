# Less-skilled immigration and natives' outcomes
# Spring 2016
# Patcha Chaikitmongkol

rm(list=ls()) # clear workspace

library("foreign")
library("AER")
library(Hmisc) # get spss (.sav) files
#library("plyr")

# Data directory 

setwd("/Users/Mint/Dropbox/Dissertation_Analysis/Data/LFS")

#----------------------------
# import LFS data 
#----------------------------

# create list of file names 
#y <- c("y07","y08","y09","y10","y11","y12","y13","y14","y15"); q <- seq(1,4); suf <- "sav"

q1 <- spss.get("y07_q1.sav", use.value.labels=TRUE)
q2 <- spss.get("y07_q2.sav", use.value.labels=TRUE)
q3 <- spss.get("y07_q3.sav", use.value.labels=TRUE)
q4 <- spss.get("y07_q4.sav", use.value.labels=TRUE)

colnum <- c(ncol(q1),ncol(q2),ncol(q3),ncol(q4)) # q3 missing one column 
colnames(q1); colnames(q3) # SUBJECT missing from q3
q3$SUBJECT <- NA # Create missing column 
lfs <- rbind(q1,q2,q3,q4)
#There's a typo in column name 
names(lfs)[names(lfs)=="RE.MK"] <- "RE.WK" # Record type of work
colnames(lfs)
rm(q1,q2,q3,q4,colnum)

q1 <- spss.get("y08_q1.sav", use.value.labels=TRUE)
q2 <- spss.get("y08_q2.sav", use.value.labels=TRUE)
q3 <- spss.get("y08_q3.sav", use.value.labels=TRUE)
q4 <- spss.get("y08_q4.sav", use.value.labels=TRUE)

colnum <- c(ncol(q1),ncol(q2),ncol(q3),ncol(q4))
colnames(lfs); colnames(q1); 
colnames(q2); colnames(q3); colnames(q4) 
q1$ABSENT <- NA # "ABSENT" (length of absence from work) missing from q1
lfs <- rbind(lfs,q1,q2,q3,q4)
rm(q1,q2,q3,q4,colnum)

q1 <- spss.get("y09_q1.sav", use.value.labels=TRUE)
q2 <- spss.get("y09_q2.sav", use.value.labels=TRUE)
q3 <- spss.get("y09_q3.sav", use.value.labels=TRUE)
q4 <- spss.get("y09_q4.sav", use.value.labels=TRUE)

colnum <- c(ncol(q1),ncol(q2),ncol(q3),ncol(q4))
colnames(lfs); colnames(q3); 
q3 <- q3[-12] #remove extra column (WHO--is surveyee a hh member?)
names(q3)[names(q3)=="TYPE."] <- "TYPE" # Record type of work
names(q3)[names(q3)=="MEMBERS."] <- "MEMBERS" # Typo: hh members
names(q3)[names(q3)=="AMOUNT"] <- "AMOUMT" # Typo: wage per unit of time specified in "WAGE.TYPE" 
names(q3)[names(q3)=="Weight"] <- "WEIGHT"
colnames(q1)
names(q1)[names(q1)=="TYPE."] <- "TYPE" # Typo: Record type of work
colnames(q2)
names(q2)[names(q2)=="TYPE."] <- "TYPE" # Typo: Record type of work
colnames(q4)
names(q4)[names(q4)=="TYPE."] <- "TYPE" # Typo: Record type of work
names(q4)[names(q4)=="MEMBERS."] <- "MEMBERS" # Typo: hh members
names(q4)[names(q4)=="AMOUNT"] <- "AMOUMT" # Typo: wage per unit of time specified in "WAGE.TYPE" 
names(q4)[names(q4)=="Weight"] <- "WEIGHT"
colnames(q1); colnames(q2); colnames(q3); colnames(q4)
lfs <- rbind(lfs,q1,q2,q3,q4)
rm(q1,q2,q3,q4,colnum)

q1 <- spss.get("y10_q1.sav", use.value.labels=TRUE)
q2 <- spss.get("y10_q2.sav", use.value.labels=TRUE)
q3 <- spss.get("y10_q3.sav", use.value.labels=TRUE)
q4 <- spss.get("y10_q4.sav", use.value.labels=TRUE)

colnum <- c(ncol(q1),ncol(q2),ncol(q3),ncol(q4))
colnames(q1); colnames(q2); colnames(q3); colnames(q4)
names(q1)[names(q1)=="TYPE."] <- "TYPE" # Record type of work
names(q1)[names(q1)=="MEMBERS."] <- "MEMBERS" # Typo: hh members
names(q2)[names(q2)=="TYPE."] <- "TYPE" # Record type of work
names(q2)[names(q2)=="MEMBERS."] <- "MEMBERS" # Typo: hh members
names(q3)[names(q3)=="TYPE."] <- "TYPE" # Record type of work
names(q3)[names(q3)=="MEMBERS."] <- "MEMBERS" # Typo: hh members
names(q4)[names(q4)=="TYPE."] <- "TYPE" # Record type of work
names(q4)[names(q4)=="MEMBERS."] <- "MEMBERS" # Typo: hh members

names(lfs)[names(lfs)=="AMOUMT"] <- "AMOUNT" # Typo: wage per unit of time specified in "WAGE.TYPE" 
names(lfs)[names(lfs)=="WEIGHT"] <- "Weight" # Typo: 

lfs <- rbind(lfs,q1,q2,q3,q4)
rm(q1,q2,q3,q4,colnum)


# PROBLEM with 2011

q1 <- read.csv(file="y11_q1.csv",head=TRUE,sep=",")
q2 <- read.csv(file="y11_q2.csv",head=TRUE,sep=",")
q3 <- read.csv(file="y11_q3.csv",head=TRUE,sep=",")
q4 <- read.csv(file="y11_q4.csv",head=TRUE,sep=",")

colnum <- c(ncol(q1),ncol(q2),ncol(q3),ncol(q4))
colnames(q1); colnames(q2); colnames(q3); colnames(q4)
names(q4)[names(q4)=="TYPE_"] <- "TYPE" # Record type of work
q2 <- q2[-19]
q2$OTH_MONEY <- NA

names(lfs)[names(lfs)=="HH.NO"] <- "HH_NO"
names(lfs)[names(lfs)=="GRADE.A"] <- "GRADE_A"
names(lfs)[names(lfs)=="GRADE.B"] <- "GRADE_B"
names(lfs)[names(lfs)=="WK.7DAY"] <- "WK_7DAY"
names(lfs)[names(lfs)=="DR.SEEK"] <- "DR_SEEK"
names(lfs)[names(lfs)=="EVER.WK"] <- "EVER_WK"
names(lfs)[names(lfs)=="RE.UNEM"] <- "RE_UNEM"
names(lfs)[names(lfs)=="DR.UNEM"] <- "DR_UNEM"
names(lfs)[names(lfs)=="MAIN.HR"] <- "MAIN_HR"
names(lfs)[names(lfs)=="OTHER.HR"] <- "OTHER_HR"
names(lfs)[names(lfs)=="TOTAL.HR"] <- "TOTAL_HR"
names(lfs)[names(lfs)=="WAGE.TYPE"] <- "WAGE_TYPE"
names(lfs)[names(lfs)=="OTH.MONEY"] <- "OTH_MONEY"
names(lfs)[names(lfs)=="RE.WK"] <- "RE_WK"
names(lfs)[names(lfs)=="RE.ED"] <- "RE_ED"




lfs <- rbind(lfs,q1,q2,q3,q4)
rm(q1,q2,q3,q4,colnum) 
