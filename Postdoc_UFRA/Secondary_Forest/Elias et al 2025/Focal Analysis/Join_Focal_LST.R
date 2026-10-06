#Join Focal Land Surface Temperature
#Eduardo Q Marques 28-09-2026

library(tidyverse)

setwd("C:/Users/Workshop/Desktop/Files")
dir()

#AGB ---------------------------------------------------------------------------
lst22 = read.csv("LST_Forest_AGB_2022.csv")
lst23 = read.csv("LST_Forest_AGB_2023.csv")
lst24 = read.csv("LST_Forest_AGB_2024.csv")

lst = rbind(lst22, lst23, lst24)

#Calculanting Annual by mean
lst2 = lst |> 
  group_by(agb, year) |> 
  summarise(delta_lst = mean(delta_lst, na.rm = TRUE),
            sf_perc = mean(sf_perc, na.rm = TRUE),
            n = mean(n, na.rm = TRUE),
            .groups = 'drop') |> 
  mutate(cond = "Annual")


lst3 <- bind_rows(lst2, lst)

write.csv(lst3, "LST_Forest_AGB_all.csv", row.names = T)


lst4 = lst3 |> 
  group_by(agb, cond) |> 
  summarize(delta_lst = mean(delta_lst))

ggplot(lst4, aes(x=agb, y=delta_lst, col = cond))+
  geom_point()+
  geom_smooth()

#Age ---------------------------------------------------------------------------
lst22 = read.csv("LST_Forest_age_2022.csv")
lst23 = read.csv("LST_Forest_age_2023.csv")
lst24 = read.csv("LST_Forest_age_2024.csv")

lst = rbind(lst22, lst23, lst24)

#Calculanting Annual by mean
lst2 = lst |> 
  group_by(age, year) |> 
  summarise(delta_lst = mean(delta_lst, na.rm = TRUE),
            sf_perc = mean(sf_perc, na.rm = TRUE),
            n = mean(n, na.rm = TRUE),
            .groups = 'drop') |> 
  mutate(cond = "Annual")


lst3 <- bind_rows(lst2, lst)

write.csv(lst3, "LST_Forest_age_all.csv", row.names = T)


lst4 = lst3 |> 
  group_by(age, cond) |> 
  summarize(delta_lst = mean(delta_lst))

ggplot(lst4, aes(x=age, y=delta_lst, col = cond))+
  geom_point()+
  geom_smooth()




