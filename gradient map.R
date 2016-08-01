#---------------------------------
# Making immigration gradient map
#---------------------------------

rm(list=ls(all=TRUE))

library(ggplot2)
#install.packages("maptools")
library(maptools)
library(rgeos)
#install.packages("Cairo")
library(Cairo)
#install.packages("ggmap")
library(ggmap)
library(scales)
library(RColorBrewer)

library("Hmisc") # get spss (.sav), csv files

# ------ import shape file ---------

setwd("/Users/Mint/Dropbox/Mint/Dissertation_Data/THA_shapefile")

tha.shp <- readShapeSpatial("THA_adm1.shp") 

class(tha.shp)
names(tha.shp) #checking name to see what's in the shapefile
print(tha.shp$NAME_1)

# fortify shapefile to get it into dataframe
tha.shp.f <- fortify(tha.shp, region="ID_1")
class(tha.shp.f) # class is now data frame
head(tha.shp.f) # each row is now lat and long

# ------ Import immigrant data ---------

imm.dat <- csv.get("/Users/Mint/Dropbox/Mint/Dissertation_Data/Imm_dat/limm_moments.txt",sep="\t") # import data

# merge cwt from imm data to prov id from shapefile
id <- data.frame(tha.shp$NAME_1,tha.shp$ID_1)
colnames(id) <- c("prov","id")

cwt.code <- csv.get("/Users/Mint/Dropbox/Mint/Dissertation_Data/Province-CWTcode.csv",sep=",") # import cwt code with prov name
colnames(cwt.code) <- c("prov","cwt")
id.cwt <- merge(id,cwt.code,by=c("prov")) # merge id and cwt code by province name 

imm.dat <- merge(imm.dat,id.cwt,by=c("cwt")) # merge id into immigrant data

data <- imm.dat[imm.dat$year==2015 & imm.dat$qtr==1,]
# data <- data[order(imm07.dat$id),]# sort data by id
# print(tha.shp$ID_1); print(imm07.dat$id)
# id = 39  (Phatthalung (Songkhla Lake)) and 64 (Songkhla (Songkhla Lake)) are missing from imm data 
# Create empty rows for these missing places
data <- rbind(data,c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,39))
data <- rbind(data,c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,64))
data <- data[,-(7:12)]; data <- data[,-(1:4)] # keep only columns of data we need

# ---- merge shapefile and dataset ----
merge.shp.coef <- merge(tha.shp.f, data, by="id", all.x=TRUE) # merge two dataframes by id
final.plot <- merge.shp.coef[order(merge.shp.coef$order), ] # order the data by order variable

# --- plot -----
ggplot() +
  geom_polygon(data = final.plot, 
               aes(x = long, y = lat, group = group, fill = limm.max), 
               color = "black", size = 0.01) + 
  coord_map()+
  scale_fill_distiller(name="Immigrants", palette = "YlGn", trans="reverse",guide = "colourbar", breaks = pretty_breaks(n = 5))+
  theme_nothing(legend = TRUE)+
  labs(title="CLM immigrants in Q1 of 2015")