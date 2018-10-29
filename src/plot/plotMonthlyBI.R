# plot monthly Breteau index
rm(list=ls(all=TRUE))

library(ggplot2)
M.FS <- read.csv("../../dat/mosquito/fengshan_mos.csv", header=T)
Mos.FS <- M.FS[M.FS$HouseHold<200,]
Mos.FS$Date2 <- as.Date(Mos.FS$Date, "%m/%d/%Y")
MOS_DF <- aggregate(Mos.FS$PosConAll,by=list((substr(Mos.FS$Date2,1,7))),sum) 
hh <- aggregate(Mos.FS$HouseHold,by=list((substr(Mos.FS$Date2,1,7))),sum) 
MOS_DF$HouseHold <- hh[,2]
MOS_DF$Month <- 1:length(MOS_DF$HouseHold)
names(MOS_DF)<-c('Date','PosConAll','HouseHold','Month')

#Mos.FS$Date2 <- as.Date(Mos.FS$Date)
MOS_DF$BI <- (MOS_DF$PosConAll/MOS_DF$HouseHold)*100
ggplot(MOS_DF, aes(Month)) + 
  geom_line(aes(y = BI))

#  scale_x_date(date_breaks = "2 year", date_labels = "%Y", limits = c(as.Date("2005-01-01"), as.Date("2016-12-01"))) +
#  xlab('Year') +
#  ylab('Monthly Case number')
