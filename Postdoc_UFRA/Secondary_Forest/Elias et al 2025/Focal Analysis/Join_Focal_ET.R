#Join Focal Evapotranpiration
#Eduardo Q Marques 28-09-2026

library(tidyverse)

setwd("C:/Users/Workshop/Desktop/Files")
dir()

#AGB ---------------------------------------------------------------------------
et22 = read.csv("ET_Forest_AGB_2022.csv")
et23 = read.csv("ET_Forest_AGB_2023.csv")
et24 = read.csv("ET_Forest_AGB_2024.csv")

et = rbind(et22, et23, et24)
et = et |> filter(cond != "Annual")

#Calculanting Annual by mean
et2 = et |> 
  group_by(agb, year) |> 
  summarise(delta_et = mean(delta_et, na.rm = TRUE),
            sf_perc = mean(sf_perc, na.rm = TRUE),
            n = mean(n, na.rm = TRUE),
            .groups = 'drop') |> 
  mutate(cond = "Annual")


et3 <- bind_rows(et2, et)

write.csv(et3, "ET_Forest_AGB_all.csv", row.names = T)


et4 = et3 |> 
  group_by(agb, cond) |> 
  summarize(delta_et = mean(delta_et))

ggplot(et4, aes(x=agb, y=delta_et, col = cond))+
  geom_point()+
  geom_smooth()

#Age ---------------------------------------------------------------------------
et22 = read.csv("ET_Forest_age_2022.csv")
et23 = read.csv("ET_Forest_age_2023.csv")
et24 = read.csv("ET_Forest_age_2024.csv")

et = rbind(et22, et23, et24)

#Calculanting Annual by mean
et2 = et |> 
  group_by(age, year) |> 
  summarise(delta_et = mean(delta_et, na.rm = TRUE),
            sf_perc = mean(sf_perc, na.rm = TRUE),
            n = mean(n, na.rm = TRUE),
            .groups = 'drop') |> 
  mutate(cond = "Annual")


et3 <- bind_rows(et2, et)

write.csv(et3, "ET_Forest_age_all.csv", row.names = T)


et4 = et3 |> 
  group_by(age, cond) |> 
  summarize(delta_et = mean(delta_et))

ggplot(et4, aes(x=age, y=delta_et, col = cond))+
  geom_point()+
  geom_smooth()




