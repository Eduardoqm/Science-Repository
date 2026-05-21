# H1 - The ERA5 performance along the successional gradient

#Eduardo Q Marques 19-05-2026

library(tidyverse)

#Load data ---------------------------------------------------------------------
setwd("G:/My Drive/Research/PosDoc_GCBC/Analises/In situ/Kestrel_and_Hobos")
dir()
master = read.csv("Master_Kestrel_Hobo_VPD_16_02_2026.csv")
era = read.csv("ERA5_CP_SecFor_VPD_2026-04-27_to_2026-05-18.csv")

#Organizing data ---------------------------------------------------------------
master$Date2 = substr(master$Date, 1, 13)
master2 = master %>% 
  group_by(Date2, Sample, Age) %>% 
  summarise(Temp_C = mean(Temp_C),
            RH = mean(RH),
            VPD = mean(VPD)) %>% 
  unite("id", Date2:Sample, remove = F)


era$Date2 = substr(era$datetime, 1, 13)
colnames(era)[3:5] = c("Temp_ERA", "RH_ERA", "VPD_ERA")
era = era %>% unite("id", Date2, Sample, remove = F)

#Joing Situ and ERA5 data ------------------------------------------------------
df = full_join(master2, era, by = "id")

df2 = df[,c(8,3,4,5,10,6,11,7,12)]
colnames(df2) = c("Date", "Sample", "Age", "Temp_situ", "Temp_ERA",
                  "RH_situ", "RH_ERA", "VPD_situ", "VPD_ERA")

df3 = df2 %>% na.omit()
df3$Date = as.POSIXct(df3$Date, format = "%Y-%m-%d %H:%M")

#Analysis ----------------------------------------------------------------------
library(mgcv)
library(lme4)

#Exemplo1
# Calcular beta para cada parcela/idade
betas <- df3 %>%
  group_by(Age) %>%
  summarise(beta = coef(lm(VPD_situ ~ VPD_ERA))[2],
            se_beta = summary(lm(VPD_situ ~ VPD_ERA))$coefficients[2,2])

# GAM para visualização (sem inferência formal)
modelo_visual <- gam(beta ~ s(Age), data = betas)
plot(modelo_visual)  # Seu gráfico


#Exemplo2
# Modelo único que dá o gráfico + inferência correta
modelo_correto <- gam(VPD_situ ~ s(Age, by = VPD_ERA, k = 4) + VPD_ERA,
                      data = df3)

summary(modelo_correto)
# O termo s(idade, by=VPD_ERA5) mostra como o efeito do ERA5 muda com a idade

################################################################################
#Exemplo3
# Se o GAM der muito trabalho, use uma abordagem mais simples:
# Primeiro, calcule os betas por idade
betas_por_idade <- df3 %>%
  group_by(Age) %>%
  summarise(beta = coef(lm(VPD_situ ~ VPD_ERA))[2],
            .groups = "drop")

# Depois modele a tendência com GAM suave (agora funciona)
modelo_simples <- gam(beta ~ s(Age, k = 3), data = betas_por_idade)
summary(modelo_simples)

# E faça o gráfico
plot(modelo_simples)

################################################################################



model1 = lmer(VPD_situ ~ VPD_ERA * Age + (1 | Sample), data = df3)
summary(model1)

beta = coef(model1)

model2 = lm(beta~s(df3$Age))







summary(lm(df3$Temp_situ~df3$Temp_ERA))
summary(lm(df3$RH_situ~df3$RH_ERA))
summary(lm(df3$VPD_situ~df3$VPD_ERA))

#Exploratory Graphics ----------------------------------------------------------
library(ggpubr)

ggplot(df3, aes(x = VPD_situ, y = VPD_ERA))+
  geom_point()+
  geom_smooth(method = lm)+
  stat_cor(col = "red")

ggplot(df3, aes(x = VPD_situ, y = VPD_ERA, col = Sample))+
  geom_point()+
  geom_smooth(method = lm)+
  stat_cor()


ggplot(df3, aes(x = VPD_situ, y = VPD_ERA))+
  geom_point()+
  geom_smooth(method = lm)+
  stat_cor(col = "red")+
  facet_wrap(~Sample, scales = "free")


ggplot(df3, aes(x = Temp_situ, y = Temp_ERA))+
  geom_point()+
  geom_smooth(method = lm)+
  stat_cor(col = "red")+
  facet_wrap(~Sample, scales = "free")


ggplot(df3, aes(x = RH_situ, y = RH_ERA))+
  geom_point()+
  geom_smooth(method = lm)+
  stat_cor(col = "red")+
  facet_wrap(~Sample, scales = "free")



ggplot(df3)+
  geom_line(aes(x = Date, y = VPD_ERA), col = "red")+
  geom_line(aes(x = Date, y = VPD_situ), col = "black")+
  facet_wrap(~Sample, ncol = 1)

ggplot(df3)+
  geom_line(aes(x = Date, y = Temp_ERA), col = "red")+
  geom_line(aes(x = Date, y = Temp_situ), col = "black")+
  facet_wrap(~Sample, scales = "free", ncol = 1)

ggplot(df3)+
  geom_line(aes(x = Date, y = RH_ERA), col = "red")+
  geom_line(aes(x = Date, y = RH_situ), col = "black")+
  facet_wrap(~Sample, scales = "free", ncol = 1)




