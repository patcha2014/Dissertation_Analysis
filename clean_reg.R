#----------------------------------------------
# Less-skilled immigration and natives' outcomes
# Spring 2016
# Patcha Chaikitmongkol
#----------------------------------------------

library("foreign")
library("AER")
library("Hmisc") # get spss (.sav), csv files
library("plyr")

rm(list=ls()) # clear workspace



# Start from importing pop size in each skill cell i, province j in each quarter --> full panel 
temp <- csv.get("/Users/Mint/Dropbox/Dissertation_Data/LFS/i_aggweightout.txt",sep="\t")
cleanreg <- temp 
colnames(cleanreg)[which(names(cleanreg) == "x")] <- "aggweight"

# Create year-qtr ID 
cleanreg$time_id <- paste(cleanreg$year,cleanreg$qtr,sep="q")


# Import weighted avg daily wage in each skill cell i, province j in each quarter
temp <- csv.get("/Users/Mint/Dropbox/Dissertation_Data/LFS/wageout.txt",sep="\t")
cleanreg <- merge(cleanreg,temp,by= c("year","qtr","reg","cwt","skill"),all.x=TRUE)
summary(cleanreg) # max weighted daily wage = 99,999 
#count(cleanreg$wage>1000) # 99 obs 
#count(cleanreg$wage>5000) # 30 obs 
#count(cleanreg$wage>10000) # 16 obs 
tapply(cleanreg$wage, cleanreg$skill, mean, na.rm=TRUE)


# Import working hours 
temp <- csv.get("/Users/Mint/Dropbox/Dissertation_Data/LFS/hours.txt",sep="\t")
cleanreg <- merge(cleanreg,temp,by= c("year","qtr","reg","cwt","skill"),all.x=TRUE)
summary(cleanreg) # max weighted daily wage = 99,999 
tapply(cleanreg$hours, cleanreg$skill, mean, na.rm=TRUE)


# Import job complexity 
temp <- csv.get("/Users/Mint/Dropbox/Dissertation_Data/LFS/ilo_comp.txt",sep="\t")
cleanreg <- merge(cleanreg,temp,by= c("year","qtr","reg","cwt","skill"),all.x=TRUE)
summary(cleanreg)  
tapply(cleanreg$ilo.comp, cleanreg$skill, mean, na.rm=TRUE)



# Import immigrant data 
temp <- csv.get("/Users/Mint/Dropbox/Dissertation_Data/Imm_dat/limm_moments.txt",sep="\t") 
cleanreg <- merge(cleanreg,temp,by= c("year","qtr","reg","cwt"),all.x=TRUE)
summary(cleanreg)




rm(list=setdiff(ls(), "cleanreg")) # remove everthing except lfs

write.table(cleanreg, "/Users/Mint/Dropbox/Dissertation_Data/cleanreg.txt",sep="\t") # output for stata
# --------- let's try reduced form -------------

library("plm")
regtest <- cleanreg
regtest$log.pop <- log(regtest$aggweight)
regtest$log.wage <- log(regtest$wage)
regtest$log.hour <- log(regtest$hours)
regtest$log.imm <- log(regtest$imm_avg)
regtest$time.id <- paste(regtest$year,regtest$qtr,sep=".")
skill.dum <- factor(regtest$skill)
fe.wage <- plm(log.wage ~ log.imm*factor(skill),index=c("cwt","time.id"), data=regtest, model="within", effect="time")
