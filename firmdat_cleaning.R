#----------------------------------------------
# Less-skilled immigration and natives' outcomes
# Summer 2016: Import firm's micro data
# Patcha Chaikitmongkol
#----------------------------------------------

rm(list=ls()) # clear workspace

library("foreign")
library("AER")
library("Hmisc") # get spss (.sav), csv files
library("plyr")


#-----Import data--------

y97 <- read.delim("/Users/Mint/Dropbox/Mint/Dissertation_Data/firmcensus/firmcensus1997/RawData/SO40.dat",header=FALSE,sep="\t")

y07 <- spss.get("/Users/Mint/Dropbox/Mint/Dissertation_Data/firmcensus/firmcensus2007/Rawdata/INDUSTRIALCENSUS2007.sav", use.value.labels=TRUE,lowernames=TRUE,charfactor=FALSE)

y12 <- spss.get("/Users/Mint/Dropbox/Mint/Dissertation_Data/firmcensus/firmcensus2012/Microdata/BIC_INDUS_55/BIC_INDUS_55.sav", use.value.labels=TRUE,lowernames=TRUE,charfactor=FALSE)


