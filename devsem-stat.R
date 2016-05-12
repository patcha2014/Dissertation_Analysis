# dev sem stats 

cleanreg <- csv.get("/Users/Mint/Dropbox/Dissertation_Data/cleanreg.txt",sep="\t")
#temp <- temp[temp$qtr==3,]
library(ggplot2) # line graph
ggplot(data=temp, aes(x = year, y = wage, group=skill, colour=skill)) + 
  geom_line() + 
  geom_point()

temp <- tapply(cleanreg$wage, cleanreg$skill, mean, na.rm=TRUE)
wage.tab <- data.frame(key=names(temp), wage=temp,options(digits=2))



interaction.plot(time_id, wage,)

library(xtable)
xtable(y)