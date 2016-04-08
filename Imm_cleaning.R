# Importing and cleaning immigrant work permit data

rm(list=ls()) # clear workspace

library("foreign")
library("AER")
library("Hmisc") # get spss (.sav), csv files
library("xlsx")
#library("plyr")

#-------------------------------------------------------
# Import imm work permit data by province, monnth, year
#-------------------------------------------------------

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

#average
limm.average.df <- aggregate(imm.mo.df$limm, list(yr = imm.mo.df$yr, qtr = imm.mo.df$qtr,reg = imm.mo.df$reg, cwt = imm.mo.df$cwt),mean,na.rm=TRUE)

#max
limm.max.df <- aggregate(imm.mo.df$limm, list(yr = imm.mo.df$yr, qtr = imm.mo.df$qtr,reg = imm.mo.df$reg, cwt = imm.mo.df$cwt),max,na.rm=TRUE)
positive <- function(x) if(x > 0) x else NA 
limm.max.df$max <- sapply(limm.max.df$x,positive)

limm.moments <- cbind(limm.average.df,limm.max.df[,"max"])
colnames(limm.moments) <- c("yr","qtr","reg","cwt","limm.avg","limm.max")


#-------export file------------
write.table(limm.moments,"limm_moments.txt",sep="\t")

#-----------------------------------------------------------------
# Import imm work permit data by countries of origin, month, year
# This will be used as an instrument 
#-----------------------------------------------------------------

# Import data
data <- read.xlsx("/Users/Mint/Dropbox/Dissertation_Data/Imm_dat/limm_by_mlc.xlsx", sheetIndex=3)
data$mya <- data$mya_l + data$mya_il # combine legals + illegals
data$lao <- data$lao_l + data$lao_il
data$cam <- data$cam_l + data$cam_il
colnames(data)
data <- data[,-(3:8)] # keep only combined data

# assign quaters
qtr <- function(x) if(x==1 | x==2 | x==3) 1 else if(x==4 | x==5 | x==6) 2 else if(x==7 | x==8 | x==9) 3 else 4
data$qtr <- sapply(data$month,qtr) # assign quarters

# find averge 
for (i in 1:length(countrylist)) {
  temp <- aggregate(data[,i+2], # mya = col 3, lao = col 4, cam = col 5
                    list(year = data$year, qtr = data$qtr),mean,na.rm=TRUE) # find mean for each qtr-yr
  colnames(temp)[which(names(temp) == "x")] <- paste("agg",paste(countrylist[i],"avg",sep="_"),sep="_")
  if (i==1) avg.limm.agg <- temp 
  else avg.limm.agg <- merge(avg.limm.agg,temp,by=c("year","qtr"))
}

# find max
for (i in 1:length(countrylist)) {
  temp <- aggregate(data[,i+2], # mya = col 3, lao = col 4, cam = col 5
                    list(year = data$year, qtr = data$qtr),max,na.rm=FALSE)
  colnames(temp)[which(names(temp) == "x")] <- paste("agg",paste(countrylist[i],"max",sep="_"),sep="_")
  if (i==1) max.limm.agg <- temp 
  else max.limm.agg <- merge(max.limm.agg,temp,by=c("year","qtr"))
}

# ------------ merge to data from first part --------------
limm.moments <- csv.get("/Users/Mint/Dropbox/Dissertation_Data/Imm_dat/limm_moments.txt",sep="\t") # import back in
colnames(limm.moments)[which(names(limm.moments) == "yr")] <- "year" # change colname yr to year
colnames(limm.moments)[which(names(limm.moments) == "limm.max")] <- "limm_max" # change colname yr to year
colnames(limm.moments)[which(names(limm.moments) == "limm.avg")] <- "limm_avg" # change colname yr to year
limm.moments <- merge(limm.moments,avg.limm.agg,by=c("year","qtr"))
limm.moments <- merge(limm.moments,max.limm.agg,by=c("year","qtr"))

#-------export file------------
write.table(limm.moments,"/Users/Mint/Dropbox/Dissertation_Data/Imm_dat/limm_moments.txt",sep="\t")


