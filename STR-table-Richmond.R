#----------------------------------------------
# Less-skilled immigration and natives' outcomes
# Spring 2016
# Patcha Chaikitmongkol
#----------------------------------------------

rm(list=ls()) # clear workspace
# rm(list=setdiff(ls(), "data")) # remove everything except data.raw

library("foreign")
library("AER")
library("Hmisc") # get spss (.sav), csv files
library("plyr")
 
setwd("/Users/Mint/Desktop") # set work directory

# Data of individuals age 16-65
data.raw <- csv.get("/Users/Mint/Dropbox/Mint/Dissertation_Data/LFS/cleaned_lfs.txt",sep="\t")

# Use only qtr 1 data in each year
data <- data.raw[data.raw$qtr==1,]

#-------------------------
# cohort identifier 
#-------------------------

# identifier for 20-24 yo cohort and 40-44 yo cohort
a = 25; b = 30
#a = 20; b = 25
c = 45; d = 50 
#c = 40; d = 45
A = paste(paste0("age", a), b, sep="-")
B = paste(paste0("age", c), d, sep="-")

cohort.fn <- function(x) if(x>=a & x<=b) A  else
                         if(x>=c & x<=d) B else NA

data$cohort <- sapply(data$age, cohort.fn)
table(data$cohort)

#-------------------------
# education identifier 
#-------------------------

# write function to identify edu level 
edulev.fn <- function(x) if(x < 400) "lessthanhigh" else 
                      if(x > 400 & x < 500) "high" else 
                      if(x > 500 & x < 600) "postsec" else 
                      if(x > 600 & x < 700) "colgrad" else 
                      if(x > 700 & x < 800) "master" else 
                      if(x > 800 & x < 900) "phd" else NA
             
# create edu level variable in the data frame
data$edulev <- sapply(data$grade.b, edulev.fn)
describe(data$grade.b); describe(data$edulev); table(data$edulev) # check new variable 

# write function to identify being in school 
enroll.fn <- function(x) if(x==1) "finished" else "inschool" 
data$enroll <- sapply(data$grade.a, enroll.fn) # create variable
describe(data$grade.a); describe(data$enroll)

#-------------------------
# industry identifier 
#-------------------------

table(data$indus) # industry variable in LFS data set

# create function to keep only first 2 digits of indus code 
# (Note: for 01-09, data appears as 1-9)

# year 2007-2011, indus 01-09 have 3 digits input, 10-99 have 4 digits input. We will remove last two digits 
# year 2012-2015, indus 01-09 have 4 digits input, 10-99 have 5 digits input. We will remove last three digits 

indus.2dg.fn <- function(x,y) if(y>=2007 & y<=2011) substr(x, 1, nchar(x)-2) else 
                              if(y>=2012 & y<=2015) substr(x, 1, nchar(x)-3) else NA 

# create 2-digit industry variable
data$indus.2dg <- mapply(indus.2dg.fn, data$indus, data$year)

# create function to assign indus name based on indus code (exclude some small indus)

indus.fn <- function(x,y) 
  if(y<=2010 & !is.na(x) & x>=1 & x<=5) "A.agri" else
    if(y<=2010 & !is.na(x) & x>=10 & x<=14) "B.mining" else
      if(y<=2010 & !is.na(x) & x>=15 & x<=37) "C.manuf" else 
        if(y<=2010 & !is.na(x) & x==45) "F.construct" else 
          if(y<=2010 & !is.na(x) & x>=50 & x<=52) "G.sales" else 
            if(y<=2010 & !is.na(x) & x>=60 & x<=63) "H.logistics_transport" else 
              if(y<=2010 & !is.na(x) & x==55) "I.hotel_restuarant" else 
                if(y<=2010 & !is.na(x) & x==64) "J.info_communication" else 
                  if(y<=2010 & !is.na(x) & x>=65 & x<=67) "K.finanservice" else 
                   #if(y<=2010 & !is.na(x) & x>=69 & x<=75) "M.professionals" else 
                    if(y<=2010 & !is.na(x) & x>=64 & x<=67) "K.finanservice" else 
                      if(y<=2010 & !is.na(x) & x==75) "O.gov_military" else    
                        if(y<=2010 & !is.na(x) & x==80) "P.eduservice" else 
                          if(y<=2010 & !is.na(x) & x==85) "Q.health_socservice" else 
                            if(y<=2010 & !is.na(x) & x==95) "T.domesticservice" else 
                              
                              if(y>2010 & !is.na(x) & x>=1 & x<=3) "A.agri" else
                                if(y>2010 & !is.na(x) & x>=5 & x<=9) "B.mining" else
                                  if(y>2010 & !is.na(x) & x>=10 & x<=33) "C.manuf" else 
                                    if(y>2010 & !is.na(x) & x>=43 & x<=45) "F.construct" else 
                                      if(y>2010 & !is.na(x) & x>=45 & x<=47) "G.sales" else 
                                        if(y>2010 & !is.na(x) & x>=49 & x<=53) "H.logistics_transport" else 
                                          if(y>2010 & !is.na(x) & x>=55 & x<=56) "I.hotel_restuarant" else 
                                            if(y>2010 & !is.na(x) & x>=58 & x<=63) "J.info_communication" else 
                                              if(y>2010 & !is.na(x) & x>=64 & x<=66) "K.finanservice" else 
                                                if(y>2010 & !is.na(x) & x>=69 & x<=75) "M.professionals" else 
                                                  if(y>2010 & !is.na(x) & x>=64 & x<=66) "K.finanservice" else 
                                                    if(y>2010 & !is.na(x) & x==84) "O.gov_military" else    
                                                      if(y>2010 & !is.na(x) & x==85) "P.eduservice" else 
                                                        if(y>2010 & !is.na(x) & x>=86 & x<=88) "Q.health_socservice" else 
                                                          if(y>2010 & !is.na(x) & x==97) "T.domesticservice" else
                                                            if(is.na(x)) NA else "Z.allothers"
                                                              

# create industry variable 
data$indus.tab <- mapply(indus.fn, as.numeric(data$indus.2dg), data$year)
describe(data$indus.2dg); describe(data$indus.tab); table(data$indus.tab) # check new variable 

# deal with unemployment
# use re.unem = reason for unemployment (1-9). Blank if not relevant 
data$indus.tab[data$re.unem>0] <- "Z.unem" # replace indus.tab with "unem" if unemployed

#-------------------------
# Save data
#-------------------------
write.table(data, "/Users/Mint/Dropbox/Mint/Dissertation_Data/LFSq1-indus-edu.txt",sep="\t")


# Import 
data <- csv.get("/Users/Mint/Dropbox/Mint/Dissertation_Data/LFSq1-indus-edu.txt",sep="\t")

#-------------------------
# Pop in each edu level by cohort 
#-------------------------

# weighted count of obs by education level 
edufrac.df <- aggregate(data$weight, by=list(year=data$year,edulev=data$edulev,cohort=data$cohort), FUN=sum) 

# sum weights to find total number of pop in each year 
# (Note: weight sys changes in year 2011)
N <- aggregate(data$weight, by=list(year=data$year,cohort=data$cohort), FUN=sum)

edufrac.df <- merge(edufrac.df, N, by=c("year","cohort")) # merge pop by edu level and total pop 
edufrac.df$fraction <- edufrac.df$x.x / edufrac.df$x.y * 100 # fraction of pop in each edu level

edufrac.df <- edufrac.df[ , -which(names(edufrac.df) %in% c("x.x","x.y"))] # remove unneccesary cols

# Create plots 
#-------------------------

# Assign variables as factors to make it goes in this order in the plots
edufrac.df$edulev <- factor(edufrac.df$edulev, levels = c("lessthanhigh","high","postsec","colgrad","master","phd"))
edufrac.df$cohort <- factor(edufrac.df$cohort, levels = c(B,A))

ggplot(edufrac.df[edufrac.df$year==2007,], aes(x = edulev, y = fraction),) +
  geom_bar(aes(fill = cohort), position = "dodge", stat="identity") + 
  xlab("\nEducation Level") +
  ylab("Fraction of population (2007)\n") + 
  guides(fill=FALSE) + 
  theme_bw() +
  coord_flip() +
  scale_y_reverse() +
  ylim(85, 0) +
  guides(fill=guide_legend(title="Cohort"))
dev.copy(png,'Edu-pop-2007.png')
dev.off()

ggplot(edufrac.df[edufrac.df$year==2015,], aes(x = edulev, y = fraction),) +
  geom_bar(aes(fill = cohort), position = "dodge", stat="identity") + 
  xlab("\nEducation Level") +
  ylab("Fraction of population (2015)\n") + 
  guides(fill=FALSE) + 
  theme_bw() +
  coord_flip() +
  ylim(0, 85) +
  guides(fill=guide_legend(title="Cohort"))
dev.copy(png,'Edu-pop-2015.png')
dev.off()

# Create table 
#-------------------------

# reshape data long to wide 
library(reshape)
temp.y <- cast(edufrac.df[edufrac.df$cohort==A,], year~edulev, value='fraction') # reshape long to wide
temp.y$cohort <- A
temp.o <- cast(edufrac.df[edufrac.df$cohort==B,], year~edulev, value='fraction') # reshape long to wide
temp.o$cohort <- B

edufrac.out <- rbind(temp.y,temp.o) # output data frame: pop by cohort, year, edu level
#rm(temp.y,temp.o,edufrac.df,N) # remove data frames 

# reorder columns
edufrac.out <- edufrac.out[c("year","cohort","lessthanhigh","high","postsec","colgrad","master","phd")]
# edufrac.out$sum <- with(edufrac.out, lessthanhigh+high+postsec+colgrad+master+phd) # check if sums up to 100



#-------------------------
# Industry x education pop
#-------------------------

eduindus.df <- aggregate(data$weight, by=list(year=data$year, indus=data$indus.tab, edulev=data$edulev, cohort=data$cohort), FUN=sum)
eduindus.df <- merge(eduindus.df, N, by=c("year","cohort"))
eduindus.df$fraction <- eduindus.df$x.x / eduindus.df$x.y * 100

eduindus.df <- eduindus.df[ , -which(names(eduindus.df) %in% c("x.x","x.y"))] # remove unneccesary cols

# Assign variables as factors to make it goes in this order in the plots
eduindus.df$edulev <- factor(eduindus.df$edulev, levels = c("lessthanhigh","high","postsec","colgrad","master","phd"))
eduindus.df$cohort <- factor(eduindus.df$cohort, levels = c(B,A))

eduindus.df$indus <- factor(eduindus.df$indus, 
                            levels = c("A.agri","B.mining","C.manuf","F.construct","G.sales",
                                       "H.logistics_transport","I.hotel_restuarant",
                                       "J.info_communication","K.finanservice",
                                       "M.professionals","O.gov_military","P.eduservice",
                                       "Q.health_socservice","T.domesticservice",
                                       "Z.allothers","Z.unem"))

# Create plots 
#-------------------------

pick.cohort = A 
#pick.cohort = B




pick.year = 2007
ggplot(eduindus.df[eduindus.df$year==pick.year & eduindus.df$cohort==pick.cohort,], aes(x=edulev, y=fraction, fill=indus),) +
    geom_bar(stat="identity") + 
    xlab("\nEducation Level") +
    ylab(paste(paste0("Fraction of workers ",pick.cohort),pick.year,sep=" ")) + 
    guides(fill=FALSE) + 
    theme_bw() +
    coord_flip() +
    scale_y_reverse() +
    ylim(85, 0) +
    guides(fill=guide_legend(title="Industry"))
dev.copy(png, paste0(paste(paste0("Edu-Indus",pick.year),pick.cohort,sep="-"),".png"))
dev.off() 
  
pick.year = 2015
ggplot(eduindus.df[eduindus.df$year==pick.year & eduindus.df$cohort==pick.cohort,], aes(x=edulev, y=fraction, fill=indus),) +
    geom_bar(stat="identity") + 
    xlab("\nEducation Level") +
    ylab(paste(paste0("Fraction of workers ",pick.cohort),pick.year,sep=" ")) + 
    guides(fill=FALSE) + 
    theme_bw() +
    coord_flip() +
    ylim(0, 85) +
    guides(fill=guide_legend(title="Industry"))
  dev.copy(png, paste0(paste(paste0("Edu-Indus",pick.year),pick.cohort,sep="-"),".png"))
  dev.off()     

