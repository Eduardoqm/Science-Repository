#Climatogram Ana Paula
#Eduardo Q Marques 21-10-2026


library(tidyverse)
library(ggpubr)

setwd("G:/My Drive/GEE_AP")
dir()

#Precipitation -----------------------------------------------------------------
df = read.csv("GPM_V07_Prec_climatograma.csv")

df2 = df[,c(2:13, 16)]

df3 <- df2 |>
  pivot_longer(
    cols = c("X0_prec_01", "X10_prec_11", "X11_prec_12", "X1_prec_02", "X2_prec_03", 
             "X3_prec_04", "X4_prec_05", "X5_prec_06", "X6_prec_07", "X7_prec_08", 
             "X8_prec_09", "X9_prec_10"),
    names_to = "Mes",          # Nome da nova coluna categórica
    values_to = "valor",       # Nome da coluna com os valores
    names_pattern = ".*_(\\d{2})$" # Captura os dois dígitos no final do nome
  ) |>
  mutate(Mes = as.numeric(Mes)) # Converte para número (1 a 12)


df3$Transicao[df3$Transicao == "ceam"] = "CEAM"
df3$Transicao[df3$Transicao == "cema"] = "CEMA"
df3$Transicao[df3$Transicao == "core"] = "CORE"

gg_prec = ggplot(df3, aes(x=Mes, y=valor, col=Transicao, fill = Transicao))+
  stat_summary(fun = mean, geom = "col", position = "dodge")+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", position = "dodge", col = "black")+
  labs(x = "Month", y = "Precipitation (mm)",
       col = NULL, fill = NULL)+
  scale_x_continuous(breaks = 1:12, 
                     labels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
                                "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))+
  theme_bw(); gg_prec

#Temperature -------------------------------------------------------------------
df_temp = read.csv("ERA5_Temp_monthly_climatograma.csv")
df_temp2 = df_temp[,c(2:13, 16)]

df_temp3 <- df_temp2 |>
  pivot_longer(
    cols = c("X0_temp01", "X10_temp11", "X11_temp12", "X1_temp02", "X2_temp03", 
             "X3_temp04", "X4_temp05", "X5_temp06", "X6_temp07", "X7_temp08", 
             "X8_temp09", "X9_temp10"),
    names_to = "Mes",          # Nome da nova coluna categórica
    values_to = "valor",       # Nome da coluna com os valores
    names_pattern = ".*p(\\d{2})$" # Captura os dois dígitos no final do nome
  ) |>
  mutate(Mes = as.numeric(Mes)) # Converte para número (1 a 12)

#Confidence Interval
df_temp3$Mes2 = as.factor(df_temp3$Mes)

df_temp3$Transicao[df_temp3$Transicao == "ceam"] = "CEAM"
df_temp3$Transicao[df_temp3$Transicao == "cema"] = "CEMA"
df_temp3$Transicao[df_temp3$Transicao == "core"] = "CORE"

gg_temp = ggplot(df_temp3, aes(x=Mes, y=valor, col=Transicao, fill = Transicao))+
  stat_summary(fun = mean, geom = "point")+
  stat_summary(fun = mean, geom = "line")+
  stat_summary(fun.data = mean_cl_boot, geom = "ribbon", alpha = 0.05)+
  labs(x = "Month", y = "Temperature (C°)",
       col = NULL, fill = NULL)+
  scale_x_continuous(breaks = 1:12, 
    labels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
               "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))+
  theme_bw(); gg_temp

#CWD ---------------------------------------------------------------------------
df_cwd = read.csv("CWD_Mensal_Historico_climatograma.csv")
df_cwd2 = df_cwd[,c(2:13, 21)]

df_cwd3 <- df_cwd2 |>
  pivot_longer(
    cols = c("CWD_01", "CWD_11", "CWD_12", "CWD_02", "CWD_03", 
             "CWD_04", "CWD_05", "CWD_06", "CWD_07", "CWD_08", 
             "CWD_09", "CWD_10"),
    names_to = "Mes",          # Nome da nova coluna categórica
    values_to = "valor",       # Nome da coluna com os valores
    names_pattern = ".*_(\\d{2})$" # Captura os dois dígitos no final do nome
  ) |>
  mutate(Mes = as.numeric(Mes)) # Converte para número (1 a 12)

df_cwd3$Transicao[df_cwd3$Transicao == "ceam"] = "CEAM"
df_cwd3$Transicao[df_cwd3$Transicao == "cema"] = "CEMA"
df_cwd3$Transicao[df_cwd3$Transicao == "core"] = "CORE"


gg_cwd = ggplot(df_cwd3, aes(x=Mes, y=valor, col=Transicao, fill = Transicao))+
  stat_summary(fun = mean, geom = "point")+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar")+
  labs(x = "Month", y = "Climatic Water Deficit (mm)",
       col = NULL, fill = NULL)+
  scale_x_continuous(breaks = 1:12, 
                     labels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
                                "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))+
  theme_bw(); gg_cwd

#All variables -----------------------------------------------------------------
ggarrange(gg_temp, gg_prec, gg_cwd,
          ncol = 1)





















df3$variable = "Accumulated Precipitation (mm)"
df_temp3$variable = "Temperature (C°)"
df_cwd3$variable = "Climatic Water Deficit (mm)"

df_full = rbind(df3, df_temp3, df_cwd3)

df_full$Mes2 = as.factor(df_full$Mes)

df_full$Transicao[df_full$Transicao == "ceam"] = "CEAM"
df_full$Transicao[df_full$Transicao == "cema"] = "CEMA"
df_full$Transicao[df_full$Transicao == "core"] = "CORE"

ggplot(df_full, aes(x=Mes2, y=valor, fill=Transicao))+
  geom_col(position = "dodge")+
  labs(x = "Mês", y = NULL, fill = NULL)+
  facet_wrap(~variable, ncol = 1, scale = "free")+
  theme_bw()
















