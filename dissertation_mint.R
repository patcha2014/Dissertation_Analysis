# Less-skilled immigration and natives' outcomes
# Spring 2016
# Patcha Chaikitmongkol

rm(list=ls()) # clear workspace

library("foreign")
library("AER")
library("Hmisc") # get spss (.sav), csv files
library("plyr")

# Data directory 

setwd("/Users/Mint/Dropbox/Dissertation_Data/LFS")

#----------------------------
# import LFS data (Already done. Use lfs.txt for combined data frame. Raw files saved in lfs_raw.zip. )
#----------------------------
#----------------------------

q1 <- spss.get("y07_q1.sav", use.value.labels=TRUE,lowernames=TRUE,charfactor=FALSE)
q2 <- spss.get("y07_q2.sav", use.value.labels=TRUE,lowernames=TRUE,charfactor=FALSE)
q3 <- spss.get("y07_q3.sav", use.value.labels=TRUE,lowernames=TRUE,charfactor=FALSE)
q4 <- spss.get("y07_q4.sav", use.value.labels=TRUE,lowernames=TRUE,charfactor=FALSE)

colnum <- c(ncol(q1),ncol(q2),ncol(q3),ncol(q4)) # q3 missing one column 
colnames(q1); colnames(q3) # SUBJECT missing from q3
q3$subject <- NA # Create missing column 
lfs <- rbind(q1,q2,q3,q4)
#There's a typo in column name 
names(lfs)[names(lfs)=="re.mk"] <- "re.wk" # Record type of work
colnames(lfs)
rm(q1,q2,q3,q4,colnum)

q1 <- spss.get("y08_q1.sav", use.value.labels=TRUE,lowernames=TRUE,charfactor=FALSE)
q2 <- spss.get("y08_q2.sav", use.value.labels=TRUE,lowernames=TRUE,charfactor=FALSE)
q3 <- spss.get("y08_q3.sav", use.value.labels=TRUE,lowernames=TRUE,charfactor=FALSE)
q4 <- spss.get("y08_q4.sav", use.value.labels=TRUE,lowernames=TRUE,charfactor=FALSE)

colnum <- c(ncol(q1),ncol(q2),ncol(q3),ncol(q4))
colnames(lfs); colnames(q1); 
colnames(q2); colnames(q3); colnames(q4) 
q1$absent <- NA # "absent" (length of absence from work) missing from q1
lfs <- rbind(lfs,q1,q2,q3,q4)
rm(q1,q2,q3,q4,colnum)

q1 <- spss.get("y09_q1.sav", use.value.labels=TRUE,lowernames=TRUE,charfactor=FALSE)
q2 <- spss.get("y09_q2.sav", use.value.labels=TRUE,lowernames=TRUE,charfactor=FALSE)
q3 <- spss.get("y09_q3.sav", use.value.labels=TRUE,lowernames=TRUE,charfactor=FALSE)
q4 <- spss.get("y09_q4.sav", use.value.labels=TRUE,lowernames=TRUE,charfactor=FALSE)

colnum <- c(ncol(q1),ncol(q2),ncol(q3),ncol(q4))
colnames(lfs); colnames(q3); 
q3 <- q3[-12] #remove extra column (WHO--is surveyee a hh member?)
names(q3)[names(q3)=="type."] <- "type" # Record type of work
names(q3)[names(q3)=="members."] <- "members" # Typo: hh members
names(q3)[names(q3)=="amount"] <- "amoumt" # Typo: wage per unit of time specified in "WAGE.type" 
#names(q3)[names(q3)=="Weight"] <- "WEIGHT"
colnames(q1)
names(q1)[names(q1)=="type."] <- "type" # Typo: Record type of work
colnames(q2)
names(q2)[names(q2)=="type."] <- "type" # Typo: Record type of work
colnames(q4)
names(q4)[names(q4)=="type."] <- "type" # Typo: Record type of work
names(q4)[names(q4)=="members."] <- "members" # Typo: hh members
names(q4)[names(q4)=="amount"] <- "amoumt" # Typo: wage per unit of time specified in "WAGE.type" 
#names(q4)[names(q4)=="Weight"] <- "WEIGHT"
colnames(q1); colnames(q2); colnames(q3); colnames(q4)
lfs <- rbind(lfs,q1,q2,q3,q4)
rm(q1,q2,q3,q4,colnum)

q1 <- spss.get("y10_q1.sav", use.value.labels=TRUE,lowernames=TRUE,charfactor=FALSE)
q2 <- spss.get("y10_q2.sav", use.value.labels=TRUE,lowernames=TRUE,charfactor=FALSE)
q3 <- spss.get("y10_q3.sav", use.value.labels=TRUE,lowernames=TRUE,charfactor=FALSE)
q4 <- spss.get("y10_q4.sav", use.value.labels=TRUE,lowernames=TRUE,charfactor=FALSE)

colnum <- c(ncol(q1),ncol(q2),ncol(q3),ncol(q4))
colnames(q1); colnames(q2); colnames(q3); colnames(q4)
names(lfs)[names(lfs)=="type"] <- "type." # Record type of work
names(lfs)[names(lfs)=="members"] <- "members." # Typo: hh members
names(lfs)[names(lfs)=="amoumt"] <- "amount" # Typo: wage per unit of time specified in "WAGE.type" 

lfs <- rbind(lfs,q1,q2,q3,q4)
rm(q1,q2,q3,q4,colnum)


# PROBLEM with 2011

q1 <- csv.get(file="y11_q1.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)
q2 <- csv.get(file="y11_q2.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)
q3 <- csv.get(file="y11_q3.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)
q4 <- csv.get(file="y11_q4.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)

colnum <- c(ncol(q1),ncol(q2),ncol(q3),ncol(q4))
colnames(q1); colnames(q2); colnames(q3); colnames(q4)
names(lfs)[names(lfs)=="type."] <- "type" # Record type of work
names(lfs)[names(lfs)=="members."] <- "members" # Typo: hh members
names(q4)[names(q4)=="type."] <- "type" # Record type of work
q2 <- q2[-19]
q2$oth.money <- NA

lfs <- rbind(q1,q2,q3,q4,lfs)
rm(q1,q2,q3,q4,colnum) 


q1 <- csv.get(file="y12_q1.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)
q2 <- csv.get(file="y12_q2.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)
q3 <- csv.get(file="y12_q3.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)
q4 <- csv.get(file="y12_q4.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)

colnum <- c(ncol(q1),ncol(q2),ncol(q3),ncol(q4))
colnames(q1); colnames(q2); colnames(q3); colnames(q4)
names(q4)[names(q4)=="type."] <- "type" # Record type of work
names(q4)[names(q4)=="members."] <- "members" # Typo: hh members

lfs <- rbind(q1,q2,q3,q4,lfs)
rm(q1,q2,q3,q4,colnum) 


q1 <- csv.get(file="y13_q1.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)
q2 <- csv.get(file="y13_q2.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)
q3 <- csv.get(file="y13_q3.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)
q4 <- csv.get(file="y13_q4.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)

colnum <- c(ncol(q1),ncol(q2),ncol(q3),ncol(q4))
colnames(q1); colnames(q2); colnames(q3); colnames(q4)
q3 <- q3[-19]
names(lfs)[names(lfs)=="type"] <- "type." # Record type of work
names(lfs)[names(lfs)=="members"] <- "members." # Typo: hh members

lfs <- rbind(q1,q2,q3,q4,lfs)
rm(q1,q2,q3,q4,colnum) 


q1 <- csv.get(file="y14_q1.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)
q2 <- csv.get(file="y14_q2.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)
q3 <- csv.get(file="y14_q3.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)
q4 <- csv.get(file="y14_q4.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)

colnum <- c(ncol(q1),ncol(q2),ncol(q3),ncol(q4))
colnames(q1); colnames(q2); colnames(q3); colnames(q4)
q1 <- q1[-41]; q2 <- q2[-41]; q3 <- q3[-41]; q4 <- q4[-41]

lfs <- rbind(q1,q2,q3,q4,lfs)
rm(q1,q2,q3,q4,colnum) 


q1 <- csv.get(file="y15_q1.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)
q2 <- csv.get(file="y15_q2.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)
q3 <- csv.get(file="y15_q3.csv",head=TRUE,sep=",",lowernames=TRUE,charfactor=FALSE)

colnum <- c(ncol(q1),ncol(q2),ncol(q3))
colnames(q1); colnames(q2); colnames(q3)
q2 <- q2[-19]
q1 <- q1[-41]; q2 <- q2[-41]; q3 <- q3[-41]
lfs <- rbind(q1,q2,q3,lfs)
rm(q1,q2,q3,colnum) 

write.table(lfs, "lfs.txt",sep="\t")
rm(lfs)

#----------------------------
# Pull data
#----------------------------
setwd("/Users/Mint/Dropbox/Dissertation_Data/LFS")
lfs <- csv.get("lfs.txt",sep="\t")

#count(lfs$occup[lfs$yr==54] != 0)

wage.df <- lfs
wage.df$yr.new <- wage.df$yr + 1957 # change years e.g. from 58 to 2015
wage.df <- subset(wage.df, select = c(yr.new,month,reg,cwt,wage.type,amount,weight))


# erase obs witout wage amount
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
wage.df <- completeFun(wage.df, "amount") 
wage.df <- completeFun(wage.df, "wage.type")
#table(wage.df$wage.type)
wage.df <- wage.df[wage.df[,"wage.type"] == 2,] # keep only wage.type = daily wage (has by far largest obs.)
#colnames(wage.df)


# Create quarter identifier
quarter <- function(x) if(x==1 | x==2 | x==3) 1 else if(x==4 | x==5 | x==6) 2 else if(x==7 | x==8 | x==9) 3 else 4
wage.df$qtr <- sapply(wage.df$month,quarter) # assign quarters

# Find weighted average wage in each cell (province x quarter x year)
wage.wavg.df <- ddply(wage.df, .(cwt,qtr,yr.new),   # so by cwt invoke following function
       function(x) data.frame(wavg.wage=weighted.mean(wage.df$amount, wage.df$weight)))


test <- wage.df[1:1000,]
test2 <- ddply(test, .(qtr),   # so by cwt invoke following function
                      function(x) data.frame(x=weighted.mean(test$amount, test$weight)))
weighted.mean(test$amount, test$weight)

dat <- data.frame(assetclass=sample(LETTERS[1:5], 20, replace=TRUE), 
                                     return=rnorm(20), assets=1e7+1e7*runif(20))
ddply(dat, .(assetclass),
      function(x) data.frame(wret=weighted.mean(x$return, x$assets)))

w.avg.df <- aggregate(wage.df$amount, list(yr = wage.df$yr.new, qtr = wage.df$qtr,reg = wage.df$reg, cwt = wage.df$cwt),mean,na.rm=TRUE); colnames(w.avg.df) <- c("yr","qtr","reg","cwt","wage.avg") # compute average daily wage in each province in each quarter

rm(list=setdiff(ls(), "lfs")) # remove everthing except lfs


