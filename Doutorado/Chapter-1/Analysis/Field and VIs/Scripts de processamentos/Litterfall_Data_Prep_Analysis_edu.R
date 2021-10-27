library(lubridate)
library(tidyverse)

###########################################################
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/LAI e Liteira")
dir()
###########################################################
litter <- read_csv("1_master_liteira_area_1_jun2020.csv")
litter$date3 = as.Date(litter$date2,"%d/%m/%Y")
###########################################################
litter.sub <- litter%>%
  arrange(plot, pont, date3)%>%
  group_by(plot, pont)%>%
  mutate(L = lag(date3),
         Edge.int = if_else(pont2%in%c('A',"AA","Bo", 'AB','C'),"Borda", "Interior"),
         DaysB = as.numeric(date3 - L, units="days"),
         DaysB = ifelse(is.na(DaysB)|DaysB > 60|DaysB==0, 15, DaysB),
         DatesF = date3 - DaysB/2,
         #Treat=as.factor(plot,labels=c("Control", "B3yr", "B1yr")),
         Value = (weight_1/DaysB)*365*(1/0.48)/(100),
         # Value = (weight_1/DaysB)*365*(1/0.64)/(100),
         Value = ifelse(date3%in%c(ymd("2004-08-10"),
                                   ymd("2004-09-30")) & plot!="A", NA, Value))%>%
  ungroup()%>%
  mutate(Treat = ifelse(plot=='A', 'Controle',
                        ifelse(plot=='B', 'B3yr', 'B1yr')),
         Edge.int = ifelse(pont2%in%c('A','AA','AB', 'B','Bo','C'),'Borda','Interior'))%>%
  select(Treat, pont,Edge.int, date, DatesF, Value)%>%
  mutate(Var='Litterfall')%>%
  filter(Value < 20, Value >=0)


###########################################################
p2 <- litter.sub%>%
  ggplot(aes(x = DatesF, 
             y = Value, 
             color = Treat,
             fill = Treat))+
  geom_smooth(method = "lm", formula = y ~ poly(x, 5), size = 1)+
  #geom_smooth()+
  #  geom_smooth(span = 0.3)+
  facet_wrap(~Edge.int)+
  theme_bw()
plot(p2)

###########################################################

litter.yr <- litter.sub %>%
  mutate(Year = year(DatesF),
         Week = week(DatesF))%>%
  group_by(Treat, Edge.int, Year, pont)%>%
  summarise(Value = mean(Value, na.rm=T))%>%
  group_by(Treat, Edge.int, Year)%>%
  summarise(Value_M = Hmisc::smean.cl.boot(Value, na.rm=T)[1],
            Value_U = Hmisc::smean.cl.boot(Value, na.rm=T)[2],
            Value_L = Hmisc::smean.cl.boot(Value, na.rm=T)[3])%>%
  mutate(Date = as.Date(paste("1", "07", Year, sep = "-"), format = "%d-%m-%Y"))%>%
  filter(Year < 2020) %>% 
  ungroup()


ggplot(litter.yr, aes(x = Year, 
                      y = Value_M,
                      ymin = Value_L,
                      ymax = Value_U,
                      color = Treat,
                      fill = Treat))+
  geom_ribbon(alpha = .5, color = NA)+
  geom_line()+
  geom_point()+
  scale_color_manual(values = c("salmon", "orange", "darkgreen"))+
  scale_fill_manual(values = c("salmon", "orange", "darkgreen"))+
  theme_minimal()+
  facet_wrap(~Edge.int)

#####################################################################
litter.ttl.yr = litter.sub %>% 
  mutate(Year = year(DatesF),
         Week = week(DatesF))%>%
  group_by(Treat, Edge.int, Year)%>%
  summarise(Value = sum(Value, na.rm=T))%>%
  mutate(Date = as.Date(paste("1", "07", Year, sep = "-"), format = "%d-%m-%Y"))%>%
  filter(Year < 2020) %>% 
  ungroup()

litter.ttl.yr$ton = litter.ttl.yr$Value/1e+9
litter.ttl.yr$kilo = litter.ttl.yr$Value/1e+6
litter.ttl.yr$grama = litter.ttl.yr$Value/1000

ggplot(litter.ttl.yr, aes(x = Year, y = ton, color = Treat, fill = Treat))+
  geom_bar(stat="identity", position = "dodge")+
  scale_color_manual(values = c("salmon", "orange", "darkgreen"))+
  scale_fill_manual(values = c("salmon", "orange", "darkgreen"))+
  theme_minimal()+
  labs(y = "tonelada ha-1 year-1")+
  facet_wrap(~Edge.int) 

ggplot(litter.ttl.yr, aes(x = Year, y = kilo, color = Treat, fill = Treat))+
  geom_bar(stat="identity", position = "dodge")+
  scale_color_manual(values = c("salmon", "orange", "darkgreen"))+
  scale_fill_manual(values = c("salmon", "orange", "darkgreen"))+
  theme_minimal()+
  labs(y = "kilo ha-1 year-1")+
  facet_wrap(~Edge.int)  

ggplot(litter.ttl.yr, aes(x = Year, y = grama, color = Treat, fill = Treat))+
  geom_bar(stat="identity", position = "dodge")+
  scale_color_manual(values = c("salmon", "orange", "darkgreen"))+
  scale_fill_manual(values = c("salmon", "orange", "darkgreen"))+
  theme_minimal()+
  labs(y = "g ha-1 year-1")+
  facet_wrap(~Edge.int)  

ggplot(litter.ttl.yr, aes(x = Year, y = Value, color = Treat, fill = Treat))+
  geom_bar(stat="identity", position = "dodge")+
  scale_color_manual(values = c("salmon", "orange", "darkgreen"))+
  scale_fill_manual(values = c("salmon", "orange", "darkgreen"))+
  theme_minimal()+
  labs(y = "Mg ha-1 year-1")+
  facet_wrap(~Edge.int)  

##################################################################################

litter.sub2 = litter.sub[,c(1,3,5,6)]
litter.sub2$DatesF = substr(litter.sub2$DatesF, 1,4)
colnames(litter.sub2) = c("Tratamento","Dist","Ano","Valor")
litter.sub2$Ano = as.numeric(litter.sub2$Ano)
litter.sub2 = litter.sub2 %>% filter(Ano < 2020)


library(extrafont)
font_import()
loadfonts(device = "win", quiet = TRUE)

ggplot(litter.sub2, aes(x=Ano, y=Valor, color = Tratamento))+
  geom_smooth(aes(group=Tratamento), alpha = 0.3, size = 1)+
  geom_vline(xintercept = 2004, linetype = "dashed")+
  geom_vline(xintercept = 2011, linetype = "dashed")+
  stat_summary(geom="point", fun.data="mean_cl_boot",
               size = 2, alpha = 0.7, aes(group=Tratamento, shape = Tratamento))+
  facet_wrap(~Dist)+
  scale_color_manual(values = c("orange", "red", "blue"))+
  xlim(2004, 2020)+
  theme_minimal()+
  labs(y = "Mg ha-1 ano-1", x = NULL)+
  theme(text = element_text(family = "Times New Roman", size = 14))


litter.sub2$gramas = litter.sub2$Valor/1000

ggplot(litter.sub2, aes(x=Ano, y=gramas, color = Tratamento))+
  geom_smooth(aes(group=Tratamento), alpha = 0.3, size = 1)+
  geom_vline(xintercept = 2004, linetype = "dashed")+
  geom_vline(xintercept = 2011, linetype = "dashed")+
  stat_summary(geom="point", fun.data="mean_cl_boot",
               size = 2, alpha = 0.7, aes(group=Tratamento, shape = Tratamento))+
  facet_wrap(~Dist)+
  scale_color_manual(values = c("orange", "red", "blue"))+
  xlim(2004, 2020)+
  theme_minimal()+
  labs(y = "g ha-1 ano-1", x = NULL)+
  theme(text = element_text(family = "Times New Roman", size = 14))






