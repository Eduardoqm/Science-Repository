#TRMM and Wind Speed by Tower

#Eduardo Q Marques 07-07-2020

library(tidyverse)
library(reshape2)
library(ggplot2)
library(plotly)
library(GGally)

#Open TRMM Data ==============================================================================
setwd('C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Capitulo2/Dados cap2/TRMM')
dir()

trmm13 = read.csv('2013.csv', sep = ',')
trmm14 = read.csv('2014.csv', sep = ',')
trmm15 = read.csv('2015.csv', sep = ',')
trmm16 = read.csv('2016.csv', sep = ',')
trmm17 = read.csv('2017.csv', sep = ',')
trmm18 = read.csv('2018.csv', sep = ',')
trmm19 = read.csv('2019.csv', sep = ',')

trmm = rbind(trmm13, trmm14,trmm15,trmm16,trmm17,trmm18,trmm19)
colnames(trmm) = c('Date', 'mm')

#Change date format (OBS: as.Date dont work here!)
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
  unite(Date, c("year","month2", "day"), sep = '-')

#Open TOWER Data ===============================================================================
setwd('C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Area1-plot/Dados das torres')
dir()
tower = read.csv('Dados_Vento_Torre_Controle.csv', sep = ',')

#Manage data to mating (01-01-2014 until 30-12-2019) ==========================================
#tower = tower[c(-1:-173),]
#trmm$id = trmm$Date
#tower$id = tower$datetime
#tower = tower %>% 
#  separate(id, c("id", "time"), sep = ' ')
#tower = tower[,c(-10)]

#df = full_join(trmm, tower, by = "id")


#Plot data ====================================================================================
#TRMM
trmm$grp = c('a')

ggplot(trmm, aes(x=Date, y=mm))+
  geom_line(aes(group=grp), col = "blue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90))

#Wind Speed
ggplot(tower, aes(x=datetime, y=max_speed))+
  geom_bar(position = "dodge", stat = "identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45))


#Resume by day =================================================================================
#Total precipitation and Max Speed
#TRMM
trmm2 = trmm %>% 
  group_by(Date) %>% 
  summarise(mm = sum(mm))

#TOWER
tower$id = tower$datetime

tower2 = tower %>% 
  separate(id, c("Date", "time"), sep = ' ') %>% 
  group_by(Date) %>% 
  summarise(max_speed = max(max_speed))

df = full_join(trmm2, tower2, by = "Date")
df = na.omit(df)


ggcorr(df)
df2 = df[,c(-1)]
ggpairs(df2)

df$grp = c('a')
ggplot(df)+
  geom_line(aes(x=Date, y=mm, group=grp), col = "blue")+
  geom_line(aes(x=Date, y=max_speed, group=grp), col = "red")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90))

df3 = melt(df)
df3$Date = as.Date(df3$Date)

ggplot(df3, aes(x=Date, y=value))+
  geom_line(aes(group = grp))+
  facet_grid(rows = vars(variable), scales = "free")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90))



df4 = df3 %>% 
  separate(Date, c("y","m","d"), sep = "-") %>% 
  filter(y == 2019) %>% 
  unite(Date, c("y","m","d"), sep = "-")

#datas = as.Date(unique(df4$Date))

#df4$Date = as.Date(df4$Date) 
 
ggplot(df4, aes(x=Date, y=value))+
  geom_line(aes(group = grp))+
  facet_grid(rows = vars(variable), scales = "free")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90))
  #scale_x_date(breaks = datas,
   #            labels = scales::date_format("%Y"),
    #           limits = range(datas))




































