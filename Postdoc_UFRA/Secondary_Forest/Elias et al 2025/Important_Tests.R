plan(multisession, workers = 27)

#Calculating LST for Primary Forest
lst_pri=ifel(is.na(fr_pri),NA,lst_dry)
plot(lst_pri)

#Use focal to calculate difference in Secondary and Primary (Delta)
lst_f <- focal(lst_pri, w=61, median, na.rm=TRUE)
plot(lst_f)

lst_delta_pri=ifel(is.na(sf),NA,lst_dry-lst_f)
plot(lst_delta_pri)

resf=as.data.frame(c(sf,lst_delta_pri, sf_perc))
colnames(resf) = c("sf_age", "delta_lst", "sf_perc")
#head(resf)

#resf2=resf%>%
#  mutate(sf_age=round(sf_age,0))%>%
#  group_by(sf_age)%>%
#  summarise(lst=mean(lst,na.rm=T))
#head(resf2)
resf2 = resf %>% na.omit()



library(ggplot2)
library(tidyverse)

setwd("G:\\Meu Drive\\Postdoc_UFRA\\Papers\\Serrapilheira (Elias et al)\\Analises_Elias\\Dados\\")
dir()

lst_df = read.csv("LST_SecFor_Age_Rainy_full.csv")

df5 = lst_df %>% 
  filter(sf_perc >= 5) %>% 
  mutate(sf_age=round(sf_age,0))%>%
  group_by(sf_age)%>%
  summarise(lst=mean(delta_lst,na.rm=T))

df30 = lst_df %>%
  filter(sf_perc >= 30)%>% 
  mutate(sf_age=round(sf_age,0))%>%
  group_by(sf_age)%>%
  summarise(lst=mean(delta_lst,na.rm=T))

df50 = lst_df%>% 
  filter(sf_perc >= 50)%>% 
  mutate(sf_age=round(sf_age,0))%>%
  group_by(sf_age)%>%
  summarise(lst=mean(delta_lst,na.rm=T))

df70 = lst_df %>% 
  filter(sf_perc >= 70)%>% 
  mutate(sf_age=round(sf_age,0))%>%
  group_by(sf_age)%>%
  summarise(lst=mean(delta_lst,na.rm=T))

df100 = lst_df %>% 
  filter(sf_perc == 100)%>% 
  mutate(sf_age=round(sf_age,0))%>%
  group_by(sf_age)%>%
  summarise(lst=mean(delta_lst,na.rm=T))


setwd("G:\\Meu Drive\\Postdoc_UFRA\\Papers\\Serrapilheira (Elias et al)\\Analises_Elias\\Figures\\")
gg5 = ggplot(df5, aes(x=sf_age, y=lst))+
  geom_point(size = 3)+
  stat_smooth()+
  labs(x="Secondary forest age (year)",y="Δ LST (C°)",
       title = "Purity >5%")+
  theme_minimal(); gg5

ggsave(plot = gg5, "Delta_LST_Amazonia_full_W61_5_perc.png", dpi = 300,
       height = 10, width = 12, units = "cm")


gg30 = ggplot(df30, aes(x=sf_age, y=lst))+
  geom_point(size = 3)+
  stat_smooth()+
  labs(x="Secondary forest age (year)",y="Δ LST (C°)",
       title = "Purity >30%")+
  theme_minimal(); gg30

ggsave(plot = gg30, "Delta_LST_Amazonia_full_W61_30_perc.png", dpi = 300,
       height = 10, width = 12, units = "cm")


gg50 = ggplot(df50, aes(x=sf_age, y=lst))+
  geom_point(size = 3)+
  stat_smooth()+
  labs(x="Secondary forest age (year)",y="Δ LST (C°)",
       title = "Purity >50%")+
  theme_minimal(); gg50

ggsave(plot = gg50, "Delta_LST_Amazonia_full_W61_50_perc.png", dpi = 300,
       height = 10, width = 12, units = "cm")

gg70 = ggplot(df70, aes(x=sf_age, y=lst))+
  geom_point(size = 3)+
  stat_smooth()+
  labs(x="Secondary forest age (year)",y="Δ LST (C°)",
       title = "Purity >70%")+
  theme_minimal(); gg70

ggsave(plot = gg70, "Delta_LST_Amazonia_full_W61_70_perc.png", dpi = 300,
       height = 10, width = 12, units = "cm")


gg100 = ggplot(df100, aes(x=sf_age, y=lst))+
  geom_point(size = 3)+
  stat_smooth()+
  labs(x="Secondary forest age (year)",y="Δ LST (C°)",
       title = "Purity >100%")+
  theme_minimal(); gg100

ggsave(plot = gg100, "Delta_LST_Amazonia_full_W61_100_perc.png", dpi = 300,
       height = 10, width = 12, units = "cm")


