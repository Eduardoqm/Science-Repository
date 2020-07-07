#TRMM Exploration (Blowdown Events - Area1) V2

#Eduardo Q Marques 10-06-2020

library(tidyverse)
library(ggplot2)
library(plotly)

#Open Data
setwd('C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Capitulo2/Dados cap2/TRMM')
dir()

trmm13 = read.csv('2013.csv', sep = ',')
trmm14 = read.csv('2014.csv', sep = ',')
trmm15 = read.csv('2015.csv', sep = ',')
trmm16 = read.csv('2016.csv', sep = ',')
trmm17 = read.csv('2017.csv', sep = ',')
trmm18 = read.csv('2018.csv', sep = ',')
trmm19 = read.csv('2019.csv', sep = ',')

trmm = rbind(trmm13,trmm14,trmm15,trmm16,trmm17,trmm18,trmm19)
colnames(trmm) = c('Date', 'mm')

#Change date format (OBS: as.Date dont work here!) ==================================
trmm = trmm %>%
  separate(Date, c("month", "day", "year"), sep = ' ')

trmm$day = gsub("[,]", "", trmm$day) #Remove comma

#Transform month names in numbers
trmm$month2[trmm$month == "Jan"] <- c('01')
trmm$month2[trmm$month == "Feb"] <- c('02')
trmm$month2[trmm$month == "Mar"] <- c('03')
trmm$month2[trmm$month == "Apr"] <- c('04')
trmm$month2[trmm$month == "May"] <- c('05')
trmm$month2[trmm$month == "Jun"] <- c('06')
trmm$month2[trmm$month == "Jul"] <- c('07')
trmm$month2[trmm$month == "Aug"] <- c('08')
trmm$month2[trmm$month == "Sep"] <- c('09')
trmm$month2[trmm$month == "Oct"] <- c('10')
trmm$month2[trmm$month == "Nov"] <- c('11')
trmm$month2[trmm$month == "Dec"] <- c('12')

#Adjust days (1-9)
trmm$day[trmm$day == "1"] <- c('01')
trmm$day[trmm$day == "2"] <- c('02')
trmm$day[trmm$day == "3"] <- c('03')
trmm$day[trmm$day == "4"] <- c('04')
trmm$day[trmm$day == "5"] <- c('05')
trmm$day[trmm$day == "6"] <- c('06')
trmm$day[trmm$day == "7"] <- c('07')
trmm$day[trmm$day == "8"] <- c('08')
trmm$day[trmm$day == "9"] <- c('09')


trmm = trmm %>%
  unite(Date, c("year","month2", "day"), sep = '')

#Plot data ==============================================================================
ggplot(trmm, aes(x=Date, y=mm))+
  geom_point(col='red', alpha = 0.4)+
  #geom_violin()+
  theme_minimal()

ggplot(trmm, aes(x=Date, y=mm))+
  geom_bar(position = "dodge", stat = "identity")+
  theme_minimal()

trmm$grp = c('a')

ggplot(trmm, aes(x=Date, y=mm))+
  geom_line(aes(group=grp), col = "blue")+
  theme_minimal()

gg = ggplot(trmm, aes(x=Date, y=mm))+
  geom_line(aes(group=grp), col = "blue")+
  theme_minimal()

inte = ggplotly(gg)




