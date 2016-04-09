#----------------------------------------------
# Less-skilled immigration and natives' outcomes
# Spring 2016
# Patcha Chaikitmongkol
#----------------------------------------------

library("Hmisc") #csv get

rm(list=ls()) # clear workspace

# Import immigrant data
imm <- csv.get("/Users/Mint/Dropbox/Dissertation_Data/Imm_dat/limm_moments.txt",sep="\t")

# Import distance data
d <- csv.get("/Users/Mint/Dropbox/Dissertation_Data/distance/directDist_fromRashesh.csv",sep=",")

# Import cwt (province code) and border crossing points' code (made up code)
provlist <- csv.get("/Users/Mint/Dropbox/Dissertation_Data/distance/prov_cwt.csv",sep=",")
xinglist <- csv.get("/Users/Mint/Dropbox/Dissertation_Data/distance/xingcode.csv",sep=",",header=TRUE)
# Note: 11-14 = Myanmar-Thailand, 21-25 = Laos-Thailand, 31-35 = Cambodia-Thailand 

# assign province and border crossing points' codes to distance data 
d <- merge(d,provlist,by=c("prov"))
d <- merge(d,xinglist,by=c("xing"))

origin <- function(x) if(x < 20) "mya" else if(x > 20 & x < 30) "lao" else "cam"
d$origin <- sapply(d$xingcode,origin) # assign country of origins

 # distance from province i to the closest crossings
d$combo <- paste(d$cwt,d$origin,sep="-")
library(plyr)   
nearest.d <- ddply(d, .(combo), # by cwt-crossings combination, 
                   summarise, cwt=cwt[which.min(d)], origin=origin[which.min(d)], # destination-origin
                   xing=xing[which.min(d)], nearest.d=min(d)) # crossing point, min distance 



