library("Hmisc") # get spss (.sav), csv files
library("xlsx")

xinglist <- read.xlsx("/Users/Mint/Dropbox/Dissertation_Data/distance/bordercrossings.xlsx", sheetIndex=1)
xinglist <- xinglist[2:15,1]
provlist <- read.xlsx("/Users/Mint/Dropbox/Dissertation_Data/distance/provlist.xlsx", sheetIndex=1)
provlist <- provlist[2:80,2]
provlist <- provlist[-2]; provlist <- provlist[-36]
bordercombo <- expand.grid(prov = provlist, xing = xinglist, KEEP.OUT.ATTRS = FALSE)

write.xlsx(bordercombo, "/Users/Mint/Dropbox/Dissertation_Data/distance/bordercombo.xlsx") # output for Rashesh
