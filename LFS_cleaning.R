#----------------------------------------------
# Less-skilled immigration and natives' outcomes
# Spring 2016
# Patcha Chaikitmongkol
#----------------------------------------------

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


vocational <- function(x) if(x==420 | x==460 | x==520) 1 else 0 # Vocational education (*don't include college level since general and vocational use the same code)
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
data$exp[is.na(data$exp)] <- 99 # change NA rows (obs with grad degree) to 99
table(data$exp);summary(data$exp)

#-----Experience cells--------

exp5 <- function(x) if(x <= 5) 1 else 0 # less than 5 years of experience
exp20 <- function(x) if(x>5 & x<=20) 1 else 0 # 5-20 years of experience
exp45 <- function(x) if(x>20 & x<=45) 1 else 0 # 20-45
exp50 <- function(x) if(x>45 & x<=50) 1 else 0 # 45-50 (Want to see if old workers are more vulnerable)
data$exp5 <- sapply(data$exp,exp5)
data$exp20 <- sapply(data$exp,exp20)
data$exp45 <- sapply(data$exp,exp45)
data$exp50 <- sapply(data$exp,exp50)
summary(data[,50:53])

#-----skill groups-----------

# less than high school edu by exp cells 
data$skill[data$lesshigh==1 & data$exp5==1] <- 11 
data$skill[data$lesshigh==1 & data$exp20==1] <- 12
data$skill[data$lesshigh==1 & data$exp45==1] <- 13
data$skill[data$lesshigh==1 & data$exp50==1] <- 14

# general high school edu by exp cells 
data$skill[data$high==1 & data$vocat == 0 & data$exp5==1] <- 21 
data$skill[data$high==1 & data$vocat == 0 & data$exp20==1] <- 22
data$skill[data$high==1 & data$vocat == 0 & data$exp45==1] <- 23
data$skill[data$high==1 & data$vocat == 0 & data$exp50==1] <- 24

# vocational (includes teacher training) high school edu by exp cells 
data$skill[data$high==1 & data$vocat == 1 & data$exp5==1] <- 211
data$skill[data$high==1 & data$vocat == 1 & data$exp20==1] <- 221
data$skill[data$high==1 & data$vocat == 1 & data$exp45==1] <- 231
data$skill[data$high==1 & data$vocat == 1 & data$exp50==1] <- 241

# general post-sec edu by exp cells 
data$skill[data$postsec==1 & data$vocat == 0 & data$exp5==1] <- 31 
data$skill[data$postsec==1 & data$vocat == 0 & data$exp20==1] <- 32
data$skill[data$postsec==1 & data$vocat == 0 & data$exp45==1] <- 33
#data$skill[data$postsec==1 & data$exp50==1] <- 34 Impossible by construction

# vocational (includes teacher training) post-sec edu by exp cells
data$skill[data$postsec==1 & data$vocat == 1 & data$exp5==1] <- 311 
data$skill[data$postsec==1 & data$vocat == 1 & data$exp20==1] <- 321
data$skill[data$postsec==1 & data$vocat == 1 & data$exp45==1] <- 331

# general college edu by exp cells 
data$skill[data$college==1 & data$exp5==1] <- 41 
data$skill[data$college==1 & data$exp20==1] <- 42
data$skill[data$college==1 & data$exp45==1] <- 43
#data$skill[data$college==1 & data$exp50==1] <- 44 Impossible by construction

data$skill[data$grad==1] <- 50

table(data$skill)


#----------------------------
# Quarter identifier 
#----------------------------

qtr <- function(x) if(x==1 | x==2 | x==3) 1 else if(x==4 | x==5 | x==6) 2 else if(x==7 | x==8 | x==9) 3 else 4
data$qtr <- sapply(data$month,qtr) # assign quarters
table(data$qtr)

write.table(data, "cleaned_lfs.txt",sep="\t") 

#----------------------------
# working age pop 
#----------------------------
# aggregate sampling weights by province in each survey quarter 
agg.weight <- aggregate(data$weight, by=list(reg=data$reg,cwt=data$cwt,qtr=data$qtr,year=data$year), FUN=sum)
# aggregate sampling weights by skill group in each province in each survey quarter 
agg.weight.i <- aggregate(data$weight, by=list(skill=data$skill,reg=data$reg,cwt=data$cwt,qtr=data$qtr,year=data$year), FUN=sum)

write.table(agg.weight.i, "i_aggweightout.txt",sep="\t") # pop output
write.table(agg.weight, "aggweightout.txt",sep="\t") # pop output


#-----------------------------------------------
# Wage by skill group i in province j at time t 
#-----------------------------------------------

wage <- data
wage <- completeFun(wage, "amount") # remove rows with empty wages
table(wage$wage.type) # 577770 obs with daily wage, 3228 obs with hrly wage, 4197 obs with wkly wage. I will keep only obs with daily wage
wage <- wage[wage$wage.type == 2,]


#------Weighted average wage in skill cell-------------

w.pooled <- ddply(subset(wage), .(reg,cwt,qtr,year),   # by (region,province,quarter,year) invoke...
          function(x) data.frame(wage=weighted.mean(x$amount, x$weight)))

skill.list <- unique(data$skill)

#for (i in 1:length(skill.list)) {
#  temp <- ddply(subset(wage,skill==skill.list[i]), .(reg,cwt,qtr,year),   
#                function(x) data.frame(temp=weighted.mean(x$amount, x$weight)))
#  if (i == 1) {wage.out <- merge(w.pooled,temp,by = c("year","qtr","reg","cwt"),all.x=TRUE)}
#  else {wage.out <- merge(wage.out,temp,by = c("year","qtr","reg","cwt"),all.x=TRUE)}
#    colnames(wage.out)[which(names(wage.out) == "temp")] <- paste("w",skill.list[i],sep="")
#}

library("gtools") #required for smartbind (rbind with diff cols)

for (i in 1:length(skill.list)) {
  temp <- ddply(subset(wage,skill==skill.list[i]), .(reg,cwt,qtr,year),   
                function(x) data.frame(wage=weighted.mean(x$amount, x$weight)))
  temp$skill <- skill.list[i] 
  if (i == 1) {wage.out <- smartbind(w.pooled,temp)}
  else {wage.out <- smartbind(wage.out,temp)}
}

# check data ----------------
summary(wage.out)
# 3208 obs have weighted avg daily wage > 1,000
# 184 ... > 10,000
check <- data[data$skill<]

write.table(wage.out, "wageout.txt",sep="\t") # wage output 

rm(list=setdiff(ls(), "data")) # remove everthing except data


#-----------------------------------------------
# Working hours by skill group i in province j at time t 
#-----------------------------------------------
# hours of worked for listed occup (main.hr)
hours <- data
hours <- completeFun(hours, "main.hr") # remove rows with no data
summary(hours$main.hr) # 98 means 98 hours and above 

for (i in 1:length(skill.list)) {
  temp <- ddply(subset(hours,skill==skill.list[i]), .(reg,cwt,qtr,year),  # for each skill,reg,cwt,qtr,year
                function(x) data.frame(hours=weighted.mean(x$main.hr, x$weight))) # find weighted avg of main.hr
  temp$skill <- skill.list[i] 
  if (i == 1) {hours.out <- temp}
  else {hours.out <- smartbind(hours.out,temp)}
}

write.table(hours.out, "hours.txt",sep="\t") # main hours output 

