library(lubridate)
library(tidyverse)

###########################################################
# setwd("~/Dropbox/Manuscript/Fire_Recovery Submission/PNAS_resubmission/Data/Figure1/LAI_Litterfall_Data")
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/LAI e Liteira")
dir()
###########################################################
# litter <- read_csv("1_master_liteira_area_1_jun2017.csv")
litter <- read_csv("1_master_liteira_area_1_jun2020.csv")
litter$date3 = as.Date(litter$date2,"%d/%m/%Y")
###########################################################
litter.sub <- litter%>%
  arrange(plot, pont, date3)%>%
  group_by(plot, pont)%>%
  mutate(L = lag(date3),
         Edge.int = if_else(pont2%in%c('A',"AA","Bo", 'AB','C'),"Edge", "Forest"),
         DaysB = as.numeric(date3 - L, units="days"),
         DaysB = ifelse(is.na(DaysB)|DaysB > 60|DaysB==0, 15, DaysB),
         DatesF = date3 - DaysB/2,
         #Treat=as.factor(plot,labels=c("Control", "B3yr", "B1yr")),
         Value = (weight_1/DaysB)*365*(1/0.48)/(100),
         # Value = (weight_1/DaysB)*365*(1/0.64)/(100),
         Value = ifelse(date3%in%c(ymd("2004-08-10"),
                                   ymd("2004-09-30")) & plot!="A", NA, Value))%>%
  ungroup()%>%
  mutate(Treat = ifelse(plot=='A', 'Control',
                        ifelse(plot=='B', 'B3yr', 'B1yr')),
         Edge.int = ifelse(pont2%in%c('A','AA','AB', 'B','Bo','C'),'Edge','Forest'))%>%
  select(Treat, pont,Edge.int, date, DatesF, Value)%>%
  mutate(Var='Litterfall')%>%
  filter(Value < 20, Value >=0)


###########################################################
p <- litter.sub%>%
  ggplot(aes(x = DatesF, 
             y = Value, color = Treat))+
  stat_summary(fun.data = "mean_cl_boot", geom = "crossbar")+
  facet_wrap(~Edge.int)+
  theme_bw()
plot(p)

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
  ungroup()


litter_export <- litter.yr%>%select(Treat, Year, Edge.int, Value_M, Value_L, Value_U)
setwd("C:/Users/leonardo.santos/Dropbox (DadosTanguro)/IPAM/eduardo/Litterfall/Datas")
dir()

write.csv(litter_export, "Fig_Litterfall.csv")
###########################################################

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
  
  
###########################################################

n = 8
res_movAVG <- litter.sub %>%
  mutate(Year = year(DatesF),
         Week = week(DatesF))%>%
  group_by(Treat, Edge.int, Year, Week)%>%
  summarise(Litterfall = Hmisc::smean.cl.boot(Value, na.rm=T)[1],
            LitterU = Hmisc::smean.cl.boot(Value, na.rm=T)[2],
            LitterL = Hmisc::smean.cl.boot(Value, na.rm=T)[3])%>%
  mutate(Date = as.Date(paste("1", Week, Year, sep = "-"), format = "%w-%W-%Y"))%>%
  #filter(Treat == "Control")%>%
  group_by(Treat, Edge.int)%>%
  mutate(Value_roll = zoo::rollmean(Litterfall, n, na.pad = T),
         Value_U = zoo::rollmean(LitterU, n, na.pad = T),
         Value_L = zoo::rollmean(LitterL, n, na.pad = T))

p5 <- res_movAVG%>%
# ggplot(aes(x = Date, y = Value_roll, color = Edge.int, fill = Edge.int))+
  ggplot(aes(x = Date, 
             y = Value_roll, 
             color = Treat, 
             fill = Treat))+
#  geom_line(lwd = 0.9, alpha = .2)+
#  geom_point(alpha = .2)+
  #  geom_pointrange(data = litter.yr, aes(x = Date, 
#                                        y = Litterfall,
#                                        ymin = LitterL,
#                                        ymax = LitterU,
#                                        color = Treat))+
  geom_ribbon(aes(ymin = Value_U,
                  ymax = Value_L), alpha = .2, colour=NA)+
#  geom_smooth(alpha = .3)+
  scale_color_manual(values = c("salmon", "orange", "darkgreen"))+
  scale_fill_manual(values = c("salmon", "orange", "darkgreen"))+
  scale_x_date(date_minor_breaks = "6 months", 
               date_breaks = "1 year",
               date_labels = "%Y")+
  geom_smooth(se = F)+
  #  facet_wrap(~Treat, ncol = 1)
  facet_wrap(~Edge.int, ncol = 1)+
  theme_minimal()

plot(p5)
###########################################################
movAVG_longM <- res_movAVG%>%ungroup()%>%
  select(Treat, Edge.int, Year, Week, Date, Value_roll)%>%
  spread(Treat, Value_roll)%>%
  mutate(CO_B1yrM = B1yr-Control,
         CO_B3yrM = B3yr-Control)

movAVG_longL <- res_movAVG%>%ungroup()%>%
  select(Treat, Edge.int, Year, Week, Date, Value_L)%>%
  spread(Treat, Value_L)%>%
  select(B1yr:Control)%>%
  rename(B1yr_L = B1yr,
         B3yr_L = B3yr,
         Control_L = Control)%>%
  mutate(CO_B1yrL = B1yr_L-Control_L,
         CO_B3yrL = B3yr_L-Control_L)

movAVG_longU <- res_movAVG%>%ungroup()%>%
  select(Treat, Edge.int, Year, Week, Date, Value_U)%>%
  spread(Treat, Value_U)%>%
  select(B1yr:Control)%>%
  rename(B1yr_U = B1yr,
         B3yr_U = B3yr,
         Control_U = Control)%>%
  mutate(CO_B1yrU = B1yr_U-Control_U,
         CO_B3yrU = B3yr_U-Control_U)

master_litter <- movAVG_longM%>%
  bind_cols(movAVG_longL)%>%
  bind_cols(movAVG_longU)
  
###########################################################
master_litter%>%
  ggplot(aes(x = Date))+
  geom_line(aes(y = CO_B1yrM), color = "orange", alpha = .5)+
  geom_line(aes(y = CO_B3yrM), color = "red", alpha = .5)+
  
  geom_ribbon(aes(ymin = CO_B1yrL,
                  ymax = CO_B1yrU), 
              fill = "orange", 
              color = NA, 
              alpha = .3)+
  geom_ribbon(aes(ymin = CO_B3yrL,
                  ymax = CO_B3yrU), 
              fill = "red", 
              color = NA, 
              alpha = .3)+
  geom_smooth(aes(y = CO_B1yrM), color = "orange", se = F)+
  geom_smooth(aes(y = CO_B3yrM), color = "red", se = F)+
  
#geom_ribbon(aes(ymin = 0,
#                  ymax = CO_B1yrM, 
#              fill = ifelse(CO_B1yrM > 0, NA, "orange")),
#              color = NA, alpha = .3)+

#  geom_ribbon(aes(ymin = 0,
#                  ymax = CO_B3yrM, 
#                  fill = ifelse(CO_B3yrM > 0, NA, "red")),
#              color = NA, alpha = .3)+
  scale_x_date(date_minor_breaks = "6 months", 
               date_breaks = "1 year",
               date_labels = "%Y")+
  geom_hline(yintercept = 0, linetype = 2, lwd = 0.9, color = "gray34")+
  facet_wrap(~Edge.int)+
  theme_minimal()
  


###########################################################


###########################################################


###########################################################


###########################################################


###########################################################


###########################################################
