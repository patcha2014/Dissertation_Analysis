# Importing and cleaning immigrant work permit data

rm(list=ls()) # clear workspace

library("foreign")
library("AER")
library("Hmisc") # get spss (.sav), csv files
library("xlsx")
#library("plyr")

#----------------------
# Importing excel files
#----------------------


month <- paste("Sheet",seq(1,12,1),sep="") # Create list of sheet names within each excel file we need to import. Sheet1 is January and so on
year <- c("2007","2008","2009","2010","2011","2012","2013","2014","2015")
filename <- paste(paste("imm",year,sep=""),"xlsx",sep=".") # List of file names. 


setwd("/Users/Mint/Dropbox/Dissertation_Data/Imm_dat")

#--------2007----------

k <- 1 
month.temp <- read.xlsx(filename[k], sheetName=month[1])
month.temp$mo <- 1 # Month identifier
year.temp <- month.temp
for (i in 2:12) {
  month.temp <- read.xlsx(filename[k], sheetName=month[i])
  month.temp$mo <- i
  year.temp <- rbind.fill(year.temp,month.temp) 
}
year.temp$yr <- year[k] # Year identifier

year.temp$cal1 <- as.numeric(as.character(year.temp$il_ht)) # Minorities
year.temp$cal2 <- as.numeric(as.character(year.temp$il_mlc)) # Myanmese, Laotian, Cambodian
year.temp$limm <- year.temp$cal1 + year.temp$cal2 # Less-skilled imm = minorities + mlc

imm.dat <- as.data.frame(cbind(year.temp$yr,year.temp$mo,year.temp$cwt,year.temp$reg,year.temp$limm))
colnames(imm.dat) <- c("yr","mo","cwt","reg","limm")

#--------2008----------

k <- 2
month.temp <- read.xlsx(filename[k], sheetName=month[1])
month.temp$mo <- 1 # Month identifier
year.temp <- month.temp
for (i in 2:12) {
  month.temp <- read.xlsx(filename[k], sheetName=month[i])
  month.temp$mo <- i
  year.temp <- rbind.fill(year.temp,month.temp) 
}
year.temp$yr <- year[k] # Year identifier

year.temp$cal1 <- as.numeric(as.character(year.temp$l_mou_im)) # MOU imports
year.temp$cal2 <- as.numeric(as.character(year.temp$l_mou_nat)) # MOU proof of nationality
year.temp$cal3 <- as.numeric(as.character(year.temp$il_ht)) # Minorities
year.temp$cal4 <- as.numeric(as.character(year.temp$il_mlc)) # Myanmese, Laotian, Cambodian
year.temp$limm <- year.temp$cal1 + year.temp$cal2 + year.temp$cal3 + year.temp$cal4

imm.dat.temp <- as.data.frame(cbind(year.temp$yr,year.temp$mo,year.temp$cwt,year.temp$reg,year.temp$limm))
colnames(imm.dat.temp) <- c("yr","mo","cwt","reg","limm")
imm.dat <- rbind(imm.dat,imm.dat.temp)

#--------2009----------

k <- 3
month.temp <- read.xlsx(filename[k], sheetName=month[1])
month.temp$mo <- 1 # Month identifier
year.temp <- month.temp
for (i in 2:12) {
  month.temp <- read.xlsx(filename[k], sheetName=month[i])
  month.temp$mo <- i
  year.temp <- rbind.fill(year.temp,month.temp) 
}
year.temp$yr <- year[k] # Year identifier

year.temp$cal1 <- as.numeric(as.character(year.temp$l_mou_im)) # MOU imports
year.temp$cal2 <- as.numeric(as.character(year.temp$l_mou_nat)) # MOU proof of nationality
year.temp$cal3 <- as.numeric(as.character(year.temp$il_ht)) # Minorities
year.temp$cal4 <- as.numeric(as.character(year.temp$il_mlc)) # Myanmese, Laotian, Cambodian
year.temp$limm <- year.temp$cal1 + year.temp$cal2 + year.temp$cal3 + year.temp$cal4

imm.dat.temp <- as.data.frame(cbind(year.temp$yr,year.temp$mo,year.temp$cwt,year.temp$reg,year.temp$limm))
colnames(imm.dat.temp) <- c("yr","mo","cwt","reg","limm")
imm.dat <- rbind(imm.dat,imm.dat.temp)


#--------2010----------

k <- 4
month.temp <- read.xlsx(filename[k], sheetName=month[1])
month.temp$mo <- 1 # Month identifier
year.temp <- month.temp
for (i in 2:12) {
  month.temp <- read.xlsx(filename[k], sheetName=month[i])
  month.temp$mo <- i
  year.temp <- rbind.fill(year.temp,month.temp) 
}
year.temp$yr <- year[k] # Year identifier

year.temp$cal1 <- as.numeric(as.character(year.temp$l_mou_im)) # MOU imports
year.temp$cal2 <- as.numeric(as.character(year.temp$l_mou_nat)) # MOU proof of nationality
year.temp$cal3 <- as.numeric(as.character(year.temp$il_ht)) # Minorities
year.temp$cal4 <- as.numeric(as.character(year.temp$il_mlc)) # Myanmese, Laotian, Cambodian
year.temp$limm <- year.temp$cal1 + year.temp$cal2 + year.temp$cal3 + year.temp$cal4

imm.dat.temp <- as.data.frame(cbind(year.temp$yr,year.temp$mo,year.temp$cwt,year.temp$reg,year.temp$limm))
colnames(imm.dat.temp) <- c("yr","mo","cwt","reg","limm")
imm.dat <- rbind(imm.dat,imm.dat.temp)

#--------2011----------

k <- 5
month.temp <- read.xlsx(filename[k], sheetName=month[1])
month.temp$mo <- 1 # Month identifier
year.temp <- month.temp
for (i in 2:12) {
  month.temp <- read.xlsx(filename[k], sheetName=month[i])
  month.temp$mo <- i
  year.temp <- rbind.fill(year.temp,month.temp) 
}
year.temp$yr <- year[k] # Year identifier

year.temp$cal1 <- as.numeric(as.character(year.temp$l_mou_im)) # MOU imports
year.temp$cal2 <- as.numeric(as.character(year.temp$l_mou_nat)) # MOU proof of nationality
year.temp$cal3 <- as.numeric(as.character(year.temp$il_ht)) # Minorities
year.temp$cal4 <- as.numeric(as.character(year.temp$il_mlc)) # Myanmese, Laotian, Cambodian
year.temp$limm <- year.temp$cal1 + year.temp$cal2 + year.temp$cal3 + year.temp$cal4

imm.dat.temp <- as.data.frame(cbind(year.temp$yr,year.temp$mo,year.temp$cwt,year.temp$reg,year.temp$limm))
colnames(imm.dat.temp) <- c("yr","mo","cwt","reg","limm")
imm.dat <- rbind(imm.dat,imm.dat.temp)

#--------2012----------

k <- 6
month.temp <- read.xlsx(filename[k], sheetName=month[1])
month.temp$mo <- 1 # Month identifier
year.temp <- month.temp
for (i in 2:12) {
  month.temp <- read.xlsx(filename[k], sheetName=month[i])
  month.temp$mo <- i
  year.temp <- rbind.fill(year.temp,month.temp) 
}
year.temp$yr <- year[k] # Year identifier

year.temp$cal1 <- as.numeric(as.character(year.temp$l_mou_im)) # MOU imports
year.temp$cal2 <- as.numeric(as.character(year.temp$l_mou_nat)) # MOU proof of nationality
year.temp$cal3 <- as.numeric(as.character(year.temp$il_ht)) # Minorities
year.temp$cal4 <- as.numeric(as.character(year.temp$il_mlc)) # Myanmese, Laotian, Cambodian
year.temp$limm <- year.temp$cal1 + year.temp$cal2 + year.temp$cal3 + year.temp$cal4

imm.dat.temp <- as.data.frame(cbind(year.temp$yr,year.temp$mo,year.temp$cwt,year.temp$reg,year.temp$limm))
colnames(imm.dat.temp) <- c("yr","mo","cwt","reg","limm")
imm.dat <- rbind(imm.dat,imm.dat.temp)


#--------2013----------

k <- 7
month.temp <- read.xlsx(filename[k], sheetName=month[1])
month.temp$mo <- 1 # Month identifier
year.temp <- month.temp
for (i in 2:12) {
  month.temp <- read.xlsx(filename[k], sheetName=month[i])
  month.temp$mo <- i
  year.temp <- rbind.fill(year.temp,month.temp) 
}
year.temp$yr <- year[k] # Year identifier

year.temp$cal1 <- as.numeric(as.character(year.temp$l_mou_im)) # MOU imports
year.temp$cal2 <- as.numeric(as.character(year.temp$l_mou_nat)) # MOU proof of nationality
year.temp$cal3 <- as.numeric(as.character(year.temp$il_ht)) # Minorities
year.temp$limm <- year.temp$cal1 + year.temp$cal2 + year.temp$cal3

imm.dat.temp <- as.data.frame(cbind(year.temp$yr,year.temp$mo,year.temp$cwt,year.temp$reg,year.temp$limm))
colnames(imm.dat.temp) <- c("yr","mo","cwt","reg","limm")
imm.dat <- rbind(imm.dat,imm.dat.temp)

#--------2014----------

k <- 8
month.temp <- read.xlsx(filename[k], sheetName=month[1])
month.temp$mo <- 1 # Month identifier
year.temp <- month.temp
for (i in 2:12) {
  month.temp <- read.xlsx(filename[k], sheetName=month[i])
  month.temp$mo <- i
  year.temp <- rbind.fill(year.temp,month.temp) 
}
year.temp$yr <- year[k] # Year identifier

year.temp$cal1 <- as.numeric(as.character(year.temp$l_mou)) # MOU imports
year.temp$cal2 <- as.numeric(as.character(year.temp$l_nat)) # MOU proof of nationality
year.temp$cal3 <- as.numeric(as.character(year.temp$il_ht)) # Minorities
year.temp$limm <- year.temp$cal1 + year.temp$cal2 + year.temp$cal3 

imm.dat.temp <- as.data.frame(cbind(year.temp$yr,year.temp$mo,year.temp$cwt,year.temp$reg,year.temp$limm))
colnames(imm.dat.temp) <- c("yr","mo","cwt","reg","limm")
imm.dat <- rbind(imm.dat,imm.dat.temp)

#--------2015----------

k <- 9
month.temp <- read.xlsx(filename[k], sheetName=month[1])
month.temp$mo <- 1 # Month identifier
year.temp <- month.temp
for (i in 2:12) {
  month.temp <- read.xlsx(filename[k], sheetName=month[i])
  month.temp$mo <- i
  year.temp <- rbind.fill(year.temp,month.temp) 
}
year.temp$yr <- year[k] # Year identifier

year.temp$cal1 <- as.numeric(as.character(year.temp$mou)) # MOU imports
year.temp$cal2 <- as.numeric(as.character(year.temp$nat)) # MOU proof of nationality
year.temp$cal3 <- as.numeric(as.character(year.temp$ht)) # Minorities
year.temp$limm <- year.temp$cal1 + year.temp$cal2 + year.temp$cal3 

imm.dat.temp <- as.data.frame(cbind(year.temp$yr,year.temp$mo,year.temp$cwt,year.temp$reg,year.temp$limm))
colnames(imm.dat.temp) <- c("yr","mo","cwt","reg","limm")
imm.dat <- rbind(imm.dat,imm.dat.temp)


#-------keep only province level obs--------

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

imm.dat <- completeFun(imm.dat, "cwt") # remove rows without province ID
imm.dat <- completeFun(imm.dat, "reg") # Bkk data appeared twice in each year--one row has reg ID and the other doesn't. Remove row without reg ID. 

#-------export file------------
write.table(imm.dat,"imm.txt",sep="\t")

imm.mo.df <- csv.get("imm.txt",sep="\t") # import back in

#-------convert data to quarterly--------
quarter <- function(x) if(x==1 | x==2 | x==3) 1 else if(x==4 | x==5 | x==6) 2 else if(x==7 | x==8 | x==9) 3 else 4
imm.mo.df$qtr <- sapply(imm.mo.df$mo,quarter) # assign quarters

install.packages("reshape2")
library("reshape2")
test <- imm.mo.df[1:1000,]
test <- test[,c("qtr","yr","limm")]


library("plyr")
ddply(test, c("qtr"), summarize, outVal = mean(data))
aggregate(test$limm, list(Q = test$qtr, Y = test$yr),mean,na.rm=TRUE)
cwt <- unique(imm.mo.df$cwt)
qtr <- seq(1:4)
imm.qtr.df <- expand.grid(year,qtr <- seq(1:4),cwt)
colnames(imm.qtr.df) <- c("yr","qtr","cwt")
imm.qtr.df$limm.beg <- NA


x = data.frame(subject = c("John", "Mary"), 
               time = c(1,1),
               age = c(33,NA),
               weight = c(90, NA),
               height = c(2,2))

