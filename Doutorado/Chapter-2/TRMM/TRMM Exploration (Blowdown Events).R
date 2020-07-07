#TRMM Exploration (Blowdown Events - Area1)

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

#trmm$Date = gsub("[,]", "", trmm$Date) #Remove comma


plot(trmm)

ggplot(trmm, aes(x=Date, y=mm))+
  geom_bar(position = "dodge", stat = "identity")+
  geom_point(col='red', alpha = 0.4)+
  theme_minimal()

trmm$Date2 <- as.Date(trmm$Date,
                      format = "%B %d, %Y")

ggplot(trmm, aes(x=Date2, y=mm))+
  geom_bar(position = "dodge", stat = "identity")+
  geom_point(col='blue', alpha = 0.4)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45))


gg = ggplot(trmm, aes(x=Date2, y=mm))+
  #geom_bar(position = "dodge", stat = "identity")+
  geom_point(col='red', alpha = 0.4)+
  theme_minimal()

inte = ggplotly(gg)




