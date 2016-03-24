# Importing and cleaning immigrant work permit data

rm(list=ls()) # clear workspace

library("foreign")
library("AER")
library("Hmisc") # get spss (.sav), csv files
library("xlsx")
#library("plyr")

month <- paste("Sheet",seq(1,12,1),sep="") # Create list of sheet names within each excel file we need to import. Sheet1 is January and so on
year <- c("2007","2008","2009","2010","2011","2012","2013","2014","2015")
filename <- paste(paste("imm",year_list,sep=""),"xlsx",sep=".") # List of file names. 



#----------------------
# Importing excel files
#----------------------

setwd("/Users/Mint/Dropbox/Dissertation_Data/Imm_dat")

#--------2007----------
k <- 1 
data_temp <- read.xlsx(filename[k], sheetName=month[1])
data_temp$mo <- 1 # Month identifier
imm2007.dat <- data_temp
for (i in 2:12) {
  data_temp <- read.xlsx(filename[k], sheetName=month[i])
  data_temp$mo <- i
  imm2007.dat <- rbind.fill(imm2007.dat,data_temp) 
}
imm2007.dat$yr <- 2007 # Year identifier

imm2007.dat$cal1_ht <- as.numeric(as.character(imm2007.dat$il_ht)) # Minorities
imm2007.dat$cal2_mlc <- as.numeric(as.character(imm2007.dat$il_mlc)) # Myanmese, Laotian, Cambodian
imm2007.dat$limm <- imm2007.dat$cal1_ht + imm2007.dat$cal2_mlc # Less-skilled imm = minorities + mlc


#--------2008----------

k <- 2 
data_temp <- read.xlsx(filename[k], sheetName=month[1])
data_temp$mo <- 1 # Month identifier
imm2008.dat <- data_temp
for (i in 2:12) {
  data_temp <- read.xlsx(filename[k], sheetName=month[i])
  data_temp$mo <- i
  imm2008.dat <- rbind.fill(imm2008.dat,data_temp) 
}
imm2008.dat$yr <- 2008 # Year identifier


imm.dat.new <- data.frame(data.matrix(imm2007.dat[,-1]))
str(imm.dat.new)
imm.dat.new$limm <- imm.dat.new$il_ht + imm.dat.new$il_mlc

n <- ncol(imm2007.dat)
imm2007.dat[,5:n] <- as.numeric(as.character(imm2007.dat[,:n]))
imm2007.dat$mlc <- as.numeric(imm2007.dat$il_ht) + as.numeric(imm2007.dat$il_mlc)

imm2007_test <- read.xlsx("imm2007.xlsx", sheetName=month[1])
imm2007_test$mo <- 1 
imm2007_temp <- imm2007_test 
imm2007_test <- read.xlsx("imm2007.xlsx", sheetName=month[2])
imm2007_test$mo <- 2 
imm2007_temp <- rbind(imm2007_temp,imm2007_test)


imm2007 <- list()
for (i in 1:12) {
  imm2007[[i]] <- read.xlsx("imm2007.xlsx", sheetName=month[i])
}

imm2008 <- list()
for (i in 1:12) {
  imm2008[[i]] <- read.xlsx("imm2008.xlsx", sheetName=month[i])
}

imm2009 <- list()
for (i in 1:12) {
  imm2009[[i]] <- read.xlsx("imm2009.xlsx", sheetName=month[i])
}

imm2010 <- list()
for (i in 1:12) {
  imm2010[[i]] <- read.xlsx("imm2010.xlsx", sheetName=month[i])
}

imm2011 <- list()
for (i in 1:12) {
  imm2011[[i]] <- read.xlsx("imm2011.xlsx", sheetName=month[i])
}

imm2012 <- list()
for (i in 1:12) {
  imm2012[[i]] <- read.xlsx("imm2012.xlsx", sheetName=month[i])
}

imm2013 <- list()
for (i in 1:12) {
  imm2013[[i]] <- read.xlsx("imm2013.xlsx", sheetName=month[i])
}

imm2014 <- list()
for (i in 1:12) {
  imm2014[[i]] <- read.xlsx("imm2014.xlsx", sheetName=month[i])
}

imm2015 <- list()
for (i in 1:12) {
  imm2015[[i]] <- read.xlsx("imm2015.xlsx", sheetName=month[i])
}

#imm2007[imm2007=="-"] <- NA

# Combine all years into one list
imm.dat <- list(imm2007,imm2008,imm2009,imm2010,imm2011,imm2012,imm2013,imm2014,imm2015) 



#----------------------
# Cleaning
#----------------------

# Function to erase rows without NA in certian col
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

for (i in 1:length(year)) {
  for (j in 1:length(month)) { 
    imm.dat[[i]][[j]] <- completeFun(imm.dat[[i]][[j]], "cwt") # Keep only province level rows 
    imm.dat[[i]][[j]] <- imm.dat[[i]][[j]][-1,] # Remove duplicated rows
    imm.dat[[i]][[j]] <- imm.dat[[i]][[j]][,grep("^(NA..)",names(imm.dat[[i]][[j]]),value = TRUE, invert = TRUE)] # Remove columns with colname which include "NA.."--empty cols
  }
}
n <- ncol(imm.dat[[1]][[1]])
imm.dat[[1]][[1]][,4:n] <- as.numeric(as.character(imm.dat[[1]][[1]][,4:n]))



#----------------------
# sum MLC imm in each year
#----------------------
total <- imm.dat[[1]][[1]]$tot 
imm.dat[[1]][[1]]$tot.mlc <- imm.dat[[1]][[1]]$tot.mlc
u
for (i in 1:length(month)) {
  imm.dat[[9]][[i]][,4:colnum(imm.dat[[9]][[i]])] <- as.numeric(imm.dat[[9]][[i]][,4:colnum(imm.dat[[9]][[i]])])
  imm.dat[[9]][[i]]$mlc_sum <- imm.dat[[9]][[i]]$nat + imm.dat[[9]][[i]]$mou + imm.dat[[]]
}

