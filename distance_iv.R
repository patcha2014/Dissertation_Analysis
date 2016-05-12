#----------------------------------------------
# Less-skilled immigration and natives' outcomes
# Spring 2016
# Patcha Chaikitmongkol
#----------------------------------------------

# IV: shortest road distance to the nearest border crossings (xing) to origin country k from each province center j times total average stock of immigrants from country k to Thailand in that qtr

library("Hmisc") #csv get

rm(list=ls()) # clear workspace


#-----------------------------------------------
# Road distance
#-----------------------------------------------

# Import distance data
# Acknowledgement: Rashesh Shrestha helped constructing road distance data from each province to each border crossing point
d.df <- csv.get("/Users/Mint/Dropbox/Dissertation_Data/distance/directDist_fromRashesh.csv",sep=",",header=FALSE)
d.df <- d.df[,-1] # remove column we don't need 
colnames(d.df) <- c("prov","xing","d")

#-----------Clean up road distance data---------------

# in d column, data record as "10.00 km". Get only numbers only
trim.leading <- function (x)  sub(" km", "", x) # function to get rid of " km"
d.df$d <- sapply(d.df$d,trim.leading) # apply function
d.df$d <- as.numeric(d.df$d) # change factor to numeric 

# for some crossing province, crossing points is very near the city, in these provinces, distance is record in m instead of km. These rows now show NA. We will change this to small numbers. 
d.df$d[is.na(d.df$d)] <- 1


# Import cwt (province code) and border crossing points' code (made up code)
provlist <- csv.get("/Users/Mint/Dropbox/Dissertation_Data/distance/prov_cwt.csv",sep=",")
xinglist <- csv.get("/Users/Mint/Dropbox/Dissertation_Data/distance/xingcode.csv",sep=",",header=TRUE)
# Note: 11-14 = Myanmar-Thailand, 21-25 = Laos-Thailand, 31-35 = Cambodia-Thailand 

# assign province and border crossing points' codes to distance data 
d.df <- merge(d.df,provlist,by=c("prov"))
d.df <- merge(d.df,xinglist,by=c("xing"))

# assign origin country to each row
origin <- function(x) if(x < 20) "mya" else if(x > 20 & x < 30) "lao" else "cam"
d.df$origin <- sapply(d.df$xingcode,origin) # assign country of origins

# compute distance from province i to the closest crossings
d.df$combo <- paste(d.df$cwt,d.df$origin,sep="-")
library(plyr)   
nearest.d <- ddply(d.df, .(combo), # by cwt-crossings combination, 
                   summarise, cwt=cwt[which.min(d)], origin=origin[which.min(d)], # destination-origin
                   xing=xing[which.min(d)], nearest.d=min(d)) # crossing point, min distance 

norm.mya <- nearest.d[nearest.d$origin=="mya",]
norm.cam <- nearest.d[nearest.d$origin=="cam",]
norm.lao <- nearest.d[nearest.d$origin=="lao",]

#-------Normalization---------------------
# normalize distance from province j to nearest border crossing with country k 
# with distance from bangkok to nearest border crossing with country k 

# identify distance from bangkok to nearest border crossing with country k 
bkk.mya <- nearest.d$nearest.d[nearest.d$combo=="10-mya"] 
bkk.cam <- nearest.d$nearest.d[nearest.d$combo=="10-cam"]
bkk.lao <- nearest.d$nearest.d[nearest.d$combo=="10-lao"]

# create inverse normalized nearest distance with distance of bkk to origin = 1 for each country 
nearest.d$inv.d <-  0
nearest.d$inv.d[nearest.d$origin=="mya"] <- 1/ (nearest.d$nearest.d[nearest.d$origin=="mya"] / bkk.mya)
nearest.d$inv.d[nearest.d$origin=="cam"] <- 1/ (nearest.d$nearest.d[nearest.d$origin=="cam"] / bkk.cam)
nearest.d$inv.d[nearest.d$origin=="lao"] <- 1/ (nearest.d$nearest.d[nearest.d$origin=="lao"] / bkk.lao)

# reshape data frame
d.iv.temp <- nearest.d[c(2,3,6)] # keep only destination, origin, inv.d
library(reshape)
d.iv.temp <- reshape(d.iv.temp, timevar="origin", idvar="cwt",direction="wide")



#-----------------------------------------------
# Immigrant data 
#-----------------------------------------------

imm<- csv.get("/Users/Mint/Dropbox/Dissertation_Data/Imm_dat/limm_moments.txt",sep="\t")


temp <- imm[c(1:4,7:9)] # subsetting data to only average imm stocks by country in each qtr
summary(temp) 

# remove rows with no data on imm by origin 
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
temp <- completeFun(temp, "agg.mya.avg") 
summary(temp)

#-----------------------------------------------
# Merge distance and imm data 
#-----------------------------------------------

# merge with immigrant data
temp <- merge(temp,d.iv.temp,by="cwt")

# multiply immigrant from origin k with inverse normalized distance of province j from origin k 
temp$interact.mya <- temp$agg.mya.avg * temp$inv.d.mya
temp$interact.cam <- temp$agg.cam.avg * temp$inv.d.cam
temp$interact.lao <- temp$agg.lao.avg * temp$inv.d.lao

temp$z.border <- temp$interact.mya + temp$interact.cam + temp$interact.lao

border.iv <- temp[c(1:4,14)]


write.table(border.iv, "/Users/Mint/Dropbox/Dissertation_Data/distance/border.iv.txt",sep="\t") # output 

#-----------------------------------------------
# Test power of iv
#-----------------------------------------------

#install.packages("mvtnorm")
library(AER) ; library(foreign) ; library(mvtnorm)

test <- imm[c(1:5)]
test <- merge(test,border.iv,by=c("year","qtr","reg","cwt"))

cleanreg <- csv.get("/Users/Mint/Dropbox/Dissertation_Data/cleanreg.txt",sep="\t")
cleanreg <- merge(cleanreg,test,by=c("year","qtr","reg","cwt"))


cleanreg <- completeFun(cleanreg, "limm.avg.x"); cleanreg <- completeFun(cleanreg,"z.border"); cleanreg <- completeFun(cleanreg,"rel.native.lf")

cleanreg$log.imm <- log(cleanreg$limm.avg.x)
cleanreg$log.z.border <- log(cleanreg$z.border)
cleanreg$log.lf <- log(cleanreg$rel.native.lf)

# first stage 
fs <- lm(log.imm ~ log.lf + log.z.border, data=cleanreg)
# null first stage (exclude iv)
fn <- lm(log.imm ~ log.lf , data=cleanreg)

# simple F-test
waldtest(fs, fn)$F[2]
# F-test robust to heteroskedasticity
waldtest(fs, fn, vcov = vcovHC(fs, type="HC0"))$F[2]
# F-test robust to clustering
# Need to run r script "clusterVCV.R" first
waldtest(fs, fn, vcov = clusterVCV(cleanreg, fs, cluster1="cwt"))$F[2]
