#Analysis Secondary (Elias Paper)

#Eduardo Q Marques 27-03-2025

library(tidyverse)
library(lme4)

#Load data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serra (Elias et al)/Analises_Elias/Dados")
df = read.csv("Paper_Elias_Data.csv")

#Preparing data to analysis ----------------------------------------------------
df$Perc_priforest = df$Perc_allforest - df$Perc_secforest

df2=df%>%
  na.omit() %>% 
  dplyr::filter(LST>=24) %>% 
  filter(Water < 5) %>% 
  filter(Non_Vegetated < 5) %>%
  filter(Nat_non_forest < 5) %>% 
  filter(Perc_priforest < 96 & Perc_priforest > 0) %>% 
  group_by(Regions, Perc_secforest) %>% 
  summarise(LST = mean(LST),
            ET = mean(ET),
            Perc_agriculture = mean(Perc_agriculture),
            Perc_allforest = mean(Perc_allforest),
            Perc_priforest = mean(Perc_priforest),
            Age_secforest = mean(Age_secforest))

#Exploratory Graphics ----------------------------------------------------------
ggplot(df2, aes(x = Perc_secforest, y = LST))+
  geom_point(aes(colour = Perc_priforest), alpha = 0.5)+
  geom_smooth(method = "lm")+
  facet_wrap(Regions~.,scales = 'free')+
  scale_color_gradient(low='blue',high = 'yellow')+
  theme_minimal()

ggplot(df2, aes(x = Perc_secforest, y = ET))+
  geom_point(aes(colour = Perc_priforest), alpha = 0.5)+
  geom_smooth(method = "lm")+
  facet_wrap(Regions~.,scales = 'free')+
  scale_color_gradient(low='blue',high = 'yellow')+
  theme_minimal()

#Models ------------------------------------------------------------------------
#LST Model
m_lst=lm(LST~Perc_allforest+Perc_secforest,data=filter(df2, Regions == "SW"),
         control=lmerControl(check.nobs.vs.nlev = "ignore",
                             check.nobs.vs.rankZ = "ignore",
                             check.nobs.vs.nRE="ignore"))

summary(m_lst)
MuMIn::r.squaredGLMM(m_lst)

names(df2)

#ET Model
m_et=lm(ET~Perc_allforest+Perc_secforest,data=filter(df2, Regions == "NW"),
        control=lmerControl(check.nobs.vs.nlev = "ignore",
                            check.nobs.vs.rankZ = "ignore",
                            check.nobs.vs.nRE="ignore"))
summary(m_et)
MuMIn::r.squaredGLMM(m_et)




df3 = df2 %>% filter(Region == "SW")

ggplot(df3, aes(x = Perc_secforest, y = LST))+
  geom_point(aes(colour = Perc_priforest), alpha = 0.5)+
  geom_smooth(method = "lm")+
  scale_color_gradient(low='blue',high = 'yellow')+
  theme_minimal()

ggplot(df2, aes(x = Perc_secforest, y = ET))+
  geom_point(aes(colour = Perc_priforest), alpha = 0.5)+
  geom_smooth(method = "lm")+
  scale_color_gradie