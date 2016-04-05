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


lfs$year <- lfs$yr + 1957 # change year format e.g. from 58 to 2015
#count(lfs$occup[lfs$yr==54] != 0)


write.table(lfs, "lfs.txt",sep="\t") #save cleaned LFS file



#----------------------------
# Skill 
#----------------------------

lfs <- csv.get("/Users/Mint/Dropbox/Dissertation_Data/LFS/lfs.txt",sep="\t") #Import cleaned LFS file

data  <- lfs 
colnames(data)
summary(data$grade.b) #education levels

# function for removing rows with NAs
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

#-----------Education-----------

data <- completeFun(data, "grade.b") # remove rows with no info on edu
summary(data$grade.b) #education levels
data <- data[data$grade.b<900,] # remove rows with other/unknown levels of edu
summary(data$grade.b) #education levels


vocational <- function(x) if(x==420 | x==460 | x==520) 1 else 0 # Vocational education
data$vocat <- sapply(data$grade.b,vocational)

lesshigh <- function(x) if(x < 400) 1 else 0 # less than high school
high <- function(x) if(x > 400 & x < 500) 1 else 0 # high school
postsec <- function(x) if(x > 500 & x < 600) 1 else 0 # post-secondary education 
college <- function(x) if(x > 600 & x < 700) 1 else 0 # bachelor degree
grad <- function(x) if(x > 700) 1 else 0 # Master's and Ph.D.  
data$lesshigh <- sapply(data$grade.b,lesshigh)
data$high <- sapply(data$grade.b,high)
data$postsec <- sapply(data$grade.b,postsec)
data$college <- sapply(data$grade.b,college)
data$grad <- sapply(data$grade.b,grad)

#-----------Experience------------

data <- completeFun(data, "age") # remove rows with no info on edu
data <- data[data$age >= 16 & data$age <= 65, ] # keep only obs age btw 16-65
summary(data$age)

# generate age at which indiv enters labor market 
data$ent.age[data$lesshigh==1] <- 15 # invdiv with less than highschool enters at 15
data$ent.age[data$high==1] <- 18 # indiv with high school edu enters at 18
data$ent.age[data$postsec==1] <- 20  # indiv with post-secondary edu enters at 20
data$ent.age[data$college==1] <- 22 # indiv with college edu enters at 22
# assume indiv with grad degree got affected the same way regardless of exp level
summary(data[,41:48])

data$exp <- data$age - data$ent.age 
# 3757 obs have -1 yr of experience and less than 500 have less than -1. Change all to 0. 
data$exp[data$exp < 0] <- 0 
table(data$exp);summary(data$exp)

#-----Experience cells--------
exp5 <- function(x) if(x <= 5) 1 else 0 # less than 5 years of experience
exp20 <- function(x) if(x>5 & x<=20) 1 else 0 # 5-20 years of experience
exp45 <- function(x) if(x>20 & x<=45) 1 else 0 # 20-45
exp50 <- function(x) if(x>45 & x<=50) 1 else 0 # 45-50 (Want to see if old workers are more vulnerable)
data$exp5 <- sapply(data$exp,exp5,na.rm = FALSE)
data$exp20 <- sapply(data$exp,exp20)
data$exp45 <- sapply(data$exp,exp45)
data$exp50 <- sapply(data$exp,exp50)
summary(data[,41:49])
#-----edu-exp cells-----------
test <- data[1:2000,]
test$exp5 <- sapply(data$exp,exp5,na.rm = FALSE)

#----------------------------
# Wage
#----------------------------



wage.df <- lfs

# edu level identifier
wage.df <- completeFun(wage.df, "grade.b") # remove rows with no info on edu
low.edu <- function(x) if(x < 400) 1 else 0 # lower upper secondary
med.edu <- function(x) if(x > 400 & x < 600) 1 else 0 # upper secondary 
high.edu <- function(x) if(x >= 600) 1 else 0 
wage.df$low.edu <- sapply(wage.df$grade.b,low.edu)
wage.df$med.edu <- sapply(wage.df$grade.b,med.edu)
wage.df$high.edu <- sapply(wage.df$grade.b,high.edu)

occup.df <- completeFun(wage.df, "occup") # remove rows with no info on occupation
table(occup.df$occup)


# erase obs witout wage amount
wage.df <- completeFun(wage.df, "amount") 
wage.df <- completeFun(wage.df, "wage.type")
#table(wage.df$wage.type)
wage.df <- wage.df[wage.df[,"wage.type"] == 2,] # keep only wage.type = daily wage (has by far largest obs.)
#colnames(wage.df)

# Create quarter identifier
quarter <- function(x) if(x==1 | x==2 | x==3) 1 else if(x==4 | x==5 | x==6) 2 else if(x==7 | x==8 | x==9) 3 else 4
wage.df$qtr <- sapply(wage.df$month,quarter) # assign quarters

# Find weighted average wage in each cell 

avgwage.df <- ddply(subset(wage.df), .(reg,cwt,qtr,year),   # so by (province x quarter x year) invoke following function
                           function(x) data.frame(wage=weighted.mean(x$amount, x$weight)))

avgwage.lowedu.df <- ddply(subset(wage.df, low.edu==1), .(reg,cwt,qtr,year),   # so by (province x quarter x year) invoke following function
                      function(x) data.frame(wage_lowedu=weighted.mean(x$amount, x$weight)))

avgwage.mededu.df <- ddply(subset(wage.df, med.edu==1), .(reg,cwt,qtr,year),   # so by (province x quarter x year) invoke following function
                           function(x) data.frame(wage_mededu=weighted.mean(x$amount, x$weight)))

#avgwage.highedu.df <- ddply(subset(wage.df, (med.edu==0 & low.edu==0)), .(reg,cwt,qtr,year),   # so by (province x quarter x year) invoke following function
                           #function(x) data.frame(wage_highedu=weighted.mean(x$amount, x$weight)))

avgwage.highedu.df <- ddply(subset(wage.df, (high.edu==1)), .(reg,cwt,qtr,year),   # so by (province x quarter x year) invoke following function
                            function(x) data.frame(wage_highedu=weighted.mean(x$amount, x$weight)))


# weighted monthly income in each cell

y.df <- ddply(subset(wage.df), .(reg,cwt,qtr,year),   # so by (province x quarter x year) invoke following function
                     function(x) data.frame(y=weighted.mean(x$approx, x$weight)))

y.lowedu.df <- ddply(subset(wage.df, low.edu==1), .(reg,cwt,qtr,year),   # so by (province x quarter x year) invoke following function
                           function(x) data.frame(y_lowedu=weighted.mean(x$approx, x$weight)))

y.mededu.df <- ddply(subset(wage.df, med.edu==1), .(reg,cwt,qtr,year),   # so by (province x quarter x year) invoke following function
                           function(x) data.frame(y_mededu=weighted.mean(x$approx, x$weight)))

y.highedu.df <- ddply(subset(wage.df, (high.edu==1)), .(reg,cwt,qtr,year),   # so by (province x quarter x year) invoke following function
                            function(x) data.frame(y_highedu=weighted.mean(x$approx, x$weight)))


wage.df <- merge(avgwage.lowedu.df,avgwage.mededu.df,by = c("year","qtr","reg","cwt"))
wage.df <- merge(wage.df,avgwage.highedu.df,by = c("year","qtr","reg","cwt"))
wage.df <- merge(wage.df,avgwage.df,by = c("year","qtr","reg","cwt"))
wage.df <- merge(wage.df,y.lowedu.df,by = c("year","qtr","reg","cwt"))
wage.df <- merge(wage.df,y.mededu.df,by = c("year","qtr","reg","cwt"))
wage.df <- merge(wage.df,y.highedu.df,by = c("year","qtr","reg","cwt"))
wage.df <- merge(wage.df,y.df,by = c("year","qtr","reg","cwt"))

write.table(wage.df,"wage.txt",sep="\t")

rm(list=setdiff(ls(), "lfs")) # remove everthing except lfs

#----------------------------
# Compute income 
#----------------------------


