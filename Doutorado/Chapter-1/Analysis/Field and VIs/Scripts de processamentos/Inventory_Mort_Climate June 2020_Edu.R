#Inventory of Biomass
#Adapt from Inventory_Mort Climate June 2020

#Eduardo Q Marques 20-11-2021

library(knitr)
library(dplyr)
library(shiny)
library(tidyverse)
library(hablar)

####Functions ==============================================================================
mortFun = function(DEAD, Ni, T){
  T = T/365.5
  (1-(1-DEAD/Ni)^(1/T))}

###Function to calculate the change in size classes
recruitFun = function(X, Ni, T){
  ((1+X/Ni)^(1/T)-1)
}

###Function to calculate anoveground biomass - Mg
chavesGCB4 = function(g,d,h){
  bio = 0.0673*(g*d^2*h)^0.976
  return(bio/1000)
}

cons2 <- function(x) {
  x.temp = rle(x)
  unlist(sapply(x.temp$lengths, function(x) {return(1:x)}))}

mcwd_d = function(Value, cwd){
  cwd =  (Value-c(5*3.3)) + cwd
  cwd = ifelse(cwd>0, 0, cwd)
  return(cwd)}

mcwd_m = function(Value, cwd){
  cwd =  (Value-100) + cwd
  cwd = ifelse(cwd>0, 0, cwd)
  return(cwd)}

conv <- function(x, y,z){
  ifelse(x == "Edge" & y == "C10", z*(1/1.35),
         ifelse(x == "Edge" & y == 'C20',  z*(1/2.7),
                ifelse(x == "Edge" & y == 'C40', z*(1/10.0),
                       
                       ifelse(x == "Forest" & y == 'C10', z*(1/1.35),
                              ifelse(x == "Forest" & y == 'C20', z*(1/(1.35*2.0)),
                                     ifelse(x == "Forest" & y == 'C40', z/40.0, NA))))))
}

conv_40 <- function(x, z){
  ifelse(x == "Edge", z/10, z/40.0)}

conv_20 <- function(x, y, z){
  z.temp = z+0.0001
  ifelse(x == "Edge" & y == "C10",    z.temp/0.48, 
         ifelse(x == "Edge" & y == 'C20',    z.temp/0.95,
                ifelse(x == "Forest" & y == 'C10',  z.temp/0.48,
                       ifelse(x == "Forest" & y == 'C20',  z.temp/0.95, NA))))
}


###Importing weather data ==================================================================
path_climate <- "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Biomassa"

load(file.path(path_climate, "Climate_Engine_Tanguro.RData"))
load(file.path(path_climate, "Master_pos_processed_Brando_March_27_2020.Rdata"))


###
master_inv_cor <- master_inv_cor %>%
  ungroup() %>%
  convert(num(dap, dap_cor)) %>%
  group_by(placa) %>%
  mutate(dap_avg = mean(dap_inter, na.rm=T)) %>%
  ungroup() %>%
  mutate(dap_scale = dap_avg - mean(dap_avg, na.rm=T),
         year_scale = scale(as.numeric(yr)),
         year_fact = as.factor(yr),
         yr_f = yr + 2000,
         dap_class_f = ifelse(dap_inter >= 10, "C10", NA),
         dap_class_f = ifelse(dap_inter >= 20, "C20", dap_class_f),
         dap_class_f = ifelse(dap_inter >= 40, "C40", dap_class_f)) %>%
  left_join(clima)

#kable(glimpse(master_inv_cor))

bio_edge <- master_inv_cor %>%
  mutate(Biomass = chavesGCB4(densidade, dap_inter, altura),
         Dist = if_else(nsdist < 250, "Borda", "Interior"),
         recruit2 = if_else(recruit %in% c("i8", "r7", "r8"), 
                            "r8", as.character(recruit))) %>%
  group_by(parcela, dbh.class, Dist, yr) %>%
  summarise(Bio = sum(Biomass*mort_cor, na.rm=T)) %>%
  mutate(Bio_ha = case_when(dbh.class == "C20" & Dist == "Borda" ~ Bio/3.5,
                            dbh.class == "C20" & Dist == "Interior"~ Bio/2.0,
                            dbh.class == "C40" & Dist == "Borda" ~ Bio/12.5,
                            dbh.class == "C40" & Dist == "Interior" ~ Bio/37.5, 
                            TRUE ~ NA_real_)) %>%
  group_by(parcela, Dist, yr) %>%
  summarise(Bio = sum(Bio_ha, na.rm=T)) %>%
  mutate(parcela = ifelse(parcela=='A','Controle',ifelse(parcela=='B','B3yr','B1yr'))) %>% 
  ungroup() %>%
  filter(!is.na(Dist)) %>%
  na.omit()

###Plot ====================================================================================
bio_edge %>%
  ggplot(aes(x = yr,
             y = Bio,
             color = parcela,
             shape = parcela)) +
  geom_point() +
  #scale_fill_manual(values = c("blue","red","orange")) +
  scale_color_manual(values = c("orange","red","blue")) +
  #geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(se = TRUE, alpha = .5) +
  ylab("Biomass (Mg/ha)") +
  facet_wrap(~Dist) +
  theme_minimal() 






















