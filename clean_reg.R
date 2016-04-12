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


#--------------------------------------------------
# RHS variable: relative labor force size in each skill group 
#--------------------------------------------------

# Import aggregate weight in each skill group in each province in each survey (Start here since it gives full panel)
aggweight <- csv.get("/Users/Mint/Dropbox/Dissertation_Data/LFS/i_aggweightout.txt",sep="\t")
colnames(aggweight)[which(names(aggweight) == "x")] <- "aggweight_i"

# Import aggregate weight in each province in each survey 
temp <- csv.get("/Users/Mint/Dropbox/Dissertation_Data/LFS/aggweightout.txt",sep="\t")
aggweight <- merge (aggweight,temp,by=c("year","qtr","reg","cwt")) 
colnames(aggweight)[which(names(aggweight) == "x")] <- "aggweight"

# Labor force size (%) of skill group i relative to native labor force in each province in each quarter 
aggweight$rel_native_lf <- aggweight$aggweight_i / aggweight$aggweight * 100
tapply(aggweight$rel_native_size, aggweight$skill, mean) # summary statistics by skill groups 

cleanreg <- aggweight[-6]; cleanreg <- cleanreg[-6]

#--------------------------------------------------
# LHS variable: daily wages 
#--------------------------------------------------
# Note: include only obs which reports daily wage 

# Import weighted avg daily wage in each skill cell i, province j in each quarter
temp <- csv.get("/Users/Mint/Dropbox/Dissertation_Data/LFS/wageout.txt",sep="\t")
cleanreg <- merge(cleanreg,temp,by= c("year","qtr","reg","cwt","skill"),all.x=TRUE)
summary(cleanreg) # max weighted daily wage = 99,999 
#count(cleanreg$wage>1000) # 99 obs 
#count(cleanreg$wage>5000) # 30 obs 
#count(cleanreg$wage>10000) # 16 obs 
tapply(cleanreg$wage, cleanreg$skill, mean, na.rm=TRUE)

#--------------------------------------------------
# LHS variable: working hours per wk
#--------------------------------------------------
# Import working hours 
temp <- csv.get("/Users/Mint/Dropbox/Dissertation_Data/LFS/hours.txt",sep="\t")
cleanreg <- merge(cleanreg,temp,by= c("year","qtr","reg","cwt","skill"),all.x=TRUE)
summary(cleanreg) # max weighted daily wage = 99,999 
tapply(cleanreg$hours, cleanreg$skill, mean, na.rm=TRUE)

#--------------------------------------------------
# LHS variable: job complexity 
#--------------------------------------------------
# Note: use skill level (1-4) by ILO catagory for each ISCO major occupation groups
# Import job complexity 
temp <- csv.get("/Users/Mint/Dropbox/Dissertation_Data/LFS/ilo_comp.txt",sep="\t")
cleanreg <- merge(cleanreg,temp,by= c("year","qtr","reg","cwt","skill"),all.x=TRUE)
summary(cleanreg)  
tapply(cleanreg$ilo.comp, cleanreg$skill, mean, na.rm=TRUE)


#--------------------------------------------------
# RHS variable: Immigrants 
#--------------------------------------------------
# Import immigrant data 
temp <- csv.get("/Users/Mint/Dropbox/Dissertation_Data/Imm_dat/limm_moments.txt",sep="\t") 
cleanreg <- merge(cleanreg,temp,by= c("year","qtr","reg","cwt"),all.x=TRUE)
summary(cleanreg)


# Create year-qtr ID 
cleanreg$time_id <- paste(cleanreg$year,cleanreg$qtr,sep="q")


colnames(cleanreg) # colnames contain "."
colnames(cleanreg) <- c("year","qtr","reg","cwt","skill","rel_native_lf",
                        "wage","hours","ilo_comp","limm_avg","limm_max","agg_mya_avg",
                        "agg_lao_avg","agg_cam_avg","agg_mya_max","agg_lao_max","agg_cam_max","time_id")



rm(list=setdiff(ls(), "cleanreg")) # remove everthing except lfs

write.table(cleanreg, "/Users/Mint/Dropbox/Dissertation_Data/cleanreg.txt",sep="\t") # output for stata

write.dta(cleanreg, "/Users/Mint/Dropbox/Dissertation_Data/cleanreg.dta") # output for stata

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
