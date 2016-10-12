#----------------------------------------------
# Less-skilled immigration and natives' outcomes
# Spring 2016
# Patcha Chaikitmongkol
#----------------------------------------------

rm(list=ls()) # clear workspace
# rm(list=setdiff(ls(), "data.raw")) # remove everything except data.raw

library("foreign")
library("AER")
library("Hmisc") # get spss (.sav), csv files
library("plyr")
 
setwd("/Users/Mint/Desktop")

# Data of individuals age 16-65
data.raw <- csv.get("/Users/Mint/Dropbox/Mint/Dissertation_Data/LFS/cleaned_lfs.txt",sep="\t")
data <- data.raw

#-------------------------
# education identifier 
#-------------------------

edulev <- function(x) if(x < 400) "lessthanhigh" else 
                      if(x > 400 & x < 500) "high" else 
                      if(x > 500 & x < 600) "postsec" else 
                      if(x > 600 & x < 700) "colgrad" else 
                      if(x > 700 & x < 800) "master" else NA
             
data$edulev <- sapply(data$grade.b, edulev)

# find weighted number of ppl in each edu level by year by qtr (sample period) -----

# weighted count of obs by education level 
pop_edulev <- aggregate(data$weight, by=list(qtr=data$qtr,year=data$year,edulev=data$edulev), FUN=sum) 

# sum weights to find total number of pop in each qtr-year 
# (Note: weight sys changes in year 2011)
N <- aggregate(data$weight, by=list(qtr=data$qtr,year=data$year), FUN=sum)

pop_edulev <- merge(pop_edulev, N, by=c("qtr","year")) # merge pop by edu level and total pop 
pop_edulev$fraction <- pop_edulev$x.x / pop_edulev$x.y * 100 # fraction of pop in each edu level in working age pop (16-65)

pop_edulev <- pop_edulev[,-c(4:5)] # remove unneccesary cols

# reshape data long to wide 
library(reshape)
pop_edulev_out <- pop_edulev[pop_edulev$qtr==1,] # keep only 1st quarter data
pop_edulev_out <- pop_edulev_out[,-c(1)] # remove qtr column
pop_edulev_out <- cast(pop_edulev_out, year~edulev, value='fraction') # reshape long to wide
# reorder columns
pop_edulev_out <- pop_edulev_out[c("year","lessthanhigh","high","postsec","colgrad","master")]


#-------------------------
# industry identifier 
#-------------------------

table(data$indus) # industry variable in LFS data set

# create function to keep only first 2 digits of indus code 
# (Note: for 01-09, data appears as 1-9)

# year 2007-2011, indus 01-09 have 3 digits input, 10-99 have 4 digits input. We will remove last two digits 
# year 2012-2015, indus 01-09 have 4 digits input, 10-99 have 5 digits input. We will remove last three digits 

substr.indus <- function(x,y) if(y>=2007 & y<=2011) substr(x, 1, nchar(x)-2) else 
                            # for year 2007-2011, remove 2 last digits
                              if(y>=2012 & y<=2015) substr(x, 1, nchar(x)-3) else NA 
                            # for year 2011-2015, remove 3 last digits


# create 2-digit industry variable
data$indus_2dg <- mapply(substr.indus, data$indus, data$year)

# create function to assign indus name to industry we would like to look at 
indus <- function(x,y) 
                     if(y<=2010 & !is.na(x) & x==1) "agri" else
                     if(y<=2010 & !is.na(x) & x==5) "fisheries" else
                     if(y<=2010 & !is.na(x) & x>=15 & x<=16) "foodmanuf" else 
                     if(y<=2010 & !is.na(x) & x==18) "garment" else 
                     if(y<=2010 & !is.na(x) & x>=29 & x<=32) "electronics" else 
                     if(y<=2010 & !is.na(x) & x==34) "motors" else 
                     if(y<=2010 & !is.na(x) & x==45) "construct" else 
                     if(y<=2010 & !is.na(x) & x==52) "retails" else 
                       
                     if(y>=2011 & !is.na(x) & x==1) "agri" else
                     if(y>=2011 & !is.na(x) & x==3) "fisheries" else
                     if(y>=2011 & !is.na(x) & x>=10 & x<=11) "foodmanuf" else 
                     if(y>=2011 & !is.na(x) & x==14) "garment" else 
                     if(y>=2011 & !is.na(x) & x>=26 & x<=27) "electronics" else 
                     if(y>=2011 & !is.na(x) & x==29) "motors" else 
                     if(y>=2011 & !is.na(x) & x==41) "construct" else 
                     if(y>=2011 & !is.na(x) & x==47) "retails" else "allothers" 


data$indus_tab <- mapply(indus, data$indus_2dg, data$year)

#-----Subsetting data------ 

# change one by one
temp <- data[data$edulev == "lessthanhigh",]
#temp <- data[data$edulev == "high",]
#temp <- data[data$edulev == "postsec",]
#temp <- data[data$edulev == "colgrad",]


# Find weighted count of obs in each sample frame 
N <- aggregate(temp$weight, by=list(qtr=temp$qtr,year=temp$year), FUN=sum)

temp1 <- aggregate(temp$weight, by=list(qtr=temp$qtr, year=temp$year, indus=temp$indus_tab), FUN=sum)
temp1 <- merge(temp1, N, by=c("qtr", "year"))
temp1$fraction <- temp1$x.x / temp1$x.y * 100

temp1 <- temp1[,-c(4:5)] # remove unneccesary cols

# reshape data long to wide 
temp2 <- temp1[temp1$qtr==1,] # keep only 1st quarter data
temp2 <- temp2[,-c(1)] # remove qtr column
temp2 <- cast(temp2, indus~year, value='fraction') # reshape long to wide
# reorder rows
target <- c("agri","fisheries","foodmanuf","garment","electronics","motors","construct","retails","allothers")
temp2 <- temp2[match(target, temp2$indus),]

# Select one by one
temp2$edulev <- "Less than high school"
#temp2$edulev <- "High school"
#temp2$edulev <- "Post secondary"
#temp2$edulev <- "College grads"

# Only the first edulev category use first row, the rest use second row
indus_edulev <- temp2
#indus_edulev <- rbind(indus_edulev, temp2)


# Before this repeat all edu cat first 
indus_edulev_wide <- indus_edulev[,-c(3:9)]
colnames(indus_edulev_wide) <- c("Industry","Frac2007","Frac2015","EduLevel")

# export table 
write.table(indus_edulev_wide, "/Users/Mint/Desktop/indus_edulev.txt",sep="\t")

#----create stacked bar graphs----

# remove all others rows
indus_edulev_temp <- subset(indus_edulev_wide, indus_edulev_wide$Industry != "allothers" & indus_edulev_wide$Industry != "retails" ) 

# Make EduLevel a factor to make sure it goes in this order in the plots
indus_edulev_temp$EduLevel <- factor(indus_edulev_temp$EduLevel, levels = c("Less than high school", "High school",  "Post secondary", "College grads"))

ggplot(indus_edulev_temp, aes(x=Industry, y=Frac2007, fill=EduLevel),) +
  geom_bar(stat="identity") + 
  xlab("\nIndustry") +
  ylab("Fraction of workers (2007)\n") + 
  guides(fill=FALSE) + 
  theme_bw() +
  coord_flip() +
  guides(fill=guide_legend(title="Education Level"))
dev.copy(png,'Indus-Edu-2007.png')
dev.off()

ggplot(indus_edulev_temp, aes(x=EduLevel, y=Frac2007, fill=Industry),) +
  geom_bar(stat="identity") + 
  xlab("\nEducation Level") +
  ylab("Fraction of workers in each edu level (2007)\n") + 
  guides(fill=FALSE) + 
  theme_bw() +
  coord_flip() +
  scale_y_reverse() + 
  guides(fill=guide_legend(title="Industry"))
dev.copy(png,'Edu-Indus-2007.png')
dev.off()

ggplot(indus_edulev_temp, aes(x=EduLevel, y=Frac2015, fill=Industry),) +
  geom_bar(stat="identity") + 
  xlab("\nEducation Level") +
  ylab("Fraction of workers in each edu level (2015)\n") + 
  guides(fill=FALSE) + 
  theme_bw() +
  coord_flip() + 
  guides(fill=guide_legend(title="Industry"))
dev.copy(png,'Edu-Indus-2015.png')
dev.off()

