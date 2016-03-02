# SES Panel 

setwd("/Users/Mint/Dropbox/Dissertation_Data/SESpanel")

library(Hmisc)
q1 <- csv.get(file="y15_q1.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)
