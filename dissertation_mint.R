# Less-skilled immigration and natives' outcomes
# Spring 2016
# Patcha Chaikitmongkol

rm(list=ls()) # clear workspace

library("foreign")
library("AER")
library("Hmisc") # get spss (.sav), csv files
library("plyr")


# ---------- Import data ---------------

wage <- csv.get("/Users/Mint/Dropbox/Dissertation_Data/LFS/avgwage.txt",sep="\t")
imm <- csv.get("/Users/Mint/Dropbox/Dissertation_Data/Imm_dat/imm.qtr.txt",sep="\t")

colnames(wage)
colnames(imm)

#install.packages("rowr") # for cbind fill
#library("rowr")

#reg.df <- cbind.fill(wage,imm)
#reg.df <- cbind.fill(wage,imm)

reg.df <- merge(wage, imm, by = c("yr","qtr","reg","cwt"))
colnames(reg.df) <- c("yr","qtr","reg","cwt","wage","imm")

reg.df$survey <- paste(reg.df$yr,reg.df$qtr,sep=".")

write.dta(reg.df,"/Users/Mint/Dropbox/Dissertation_Data/reg.dta")

# --------- Reduced form reg ---------------
library(plm)
library(lmtest)






