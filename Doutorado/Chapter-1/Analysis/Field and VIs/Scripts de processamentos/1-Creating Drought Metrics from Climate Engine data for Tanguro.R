library(tidyverse)

####
####Functions
####
cons2 <- function(x) {
  x.temp = rle(x)
  unlist(sapply(x.temp$lengths, function(x) {return(1:x)}))}
###

mcwd_d = function(Value, cwd){
  cwd =  (Value-c(5*3.3)) + cwd
  cwd = ifelse(cwd>0, 0, cwd)
  return(cwd)}

mcwd_m = function(Value, cwd){
  cwd =  (Value-100) + cwd
  cwd = ifelse(cwd>0, 0, cwd)
  return(cwd)}

######################################################################################################
###Importing weather data
######################################################################################################

pa <- "/Users/paulobrando/Dropbox/Projects/Projetos_Darro/Savanizacao/ClimateEngine"
ll <- list.files("/Users/paulobrando/Dropbox/Projects/Projetos_Darro/Savanizacao/ClimateEngine")
fn <- file.path(pa, ll)

#Climate data
ce1 <- read_csv(fn[1])#Max temperature MERRA
ce2 <- read_csv(fn[2])#VPD TerraClimate
ce3 <- read_csv(fn[3])#Climate water deficit TerraClimate
ce4 <- read_csv(fn[4])#PPT TerraClimate
ce5 <- read_csv(fn[5])#PPT CHIRPS - 5 days
ce6 <- read_csv(fn[6])#Specific Humidity

######################################################################################################
###Drought metrics
######################################################################################################

mm = 9 #initial month to define hydrological year

###
max_temp <- ce1 %>% 
  rename(MT = `(deg C) Max Temperature (MERRA2) at 52.3905W,-13.0795N, 2003-01-01 to 2018-12-01`) %>%
  mutate(yr = year(DateTime),
         mo = month(DateTime),
         yr_h = ifelse(mo>mm, yr+1, yr),
         MTD = ifelse(MT >= 33, 1,0),
         MTC = cons2(MTD)*MTD)%>%
  group_by(yr_h) %>%
  summarise(MTC = max(MTC)) %>%ungroup()

###
max_vpd <- ce2 %>% 
  rename(VPD =  `(kPa) Vapor Pressure Deficit (TerraClimate) at 52.3905W,-13.0795N, 2003-01-01 to 2018-12-01`) %>%
  mutate(yr = year(DateTime),
         mo = month(DateTime),
         yr_h = ifelse(mo>mm, yr+1, yr))%>%
  group_by(yr_h) %>%
  summarise(vpd = quantile(VPD, .75)) %>%ungroup()


###
max_cwd <- ce3 %>% 
  rename(CWD =  `(mm) Climate Water Deficit (TerraClimate) at 52.3905W,-13.0795N, 2003-01-01 to 2018-12-01`) %>%
  mutate(yr = year(DateTime),
         mo = month(DateTime),
         yr_h = ifelse(mo>mm, yr+1, yr))%>%
  group_by(yr_h) %>%
  summarise(cwd_m = max(CWD, .75)) %>%ungroup()

###
max_mcwd <- ce4 %>% 
  rename(ppt =  `(mm) Precipitation (TerraClimate) at 52.3905W,-13.0795N, 2003-01-01 to 2018-12-01`) %>%
  mutate(yr = year(DateTime),
         mo = month(DateTime),
         yr_h = ifelse(mo>mm, yr+1, yr),
         cwd_c = Reduce(mcwd_m, x=ppt, accumulate = TRUE))%>%
  group_by(yr_h) %>%
  summarise(mcwd_m = min(cwd_c)) %>%ungroup()

###
max_mcwd_chirps <- ce5 %>% 
  rename(ppt =  `(mm) Precipitation (CHIRPS) at 52.3905W,-13.0795N, 2003-01-01 to 2018-12-01`) %>%
  mutate(yr = year(DateTime),
         mo = month(DateTime),
         yr_h = ifelse(mo>mm, yr+1, yr),
         cwd_c = Reduce(mcwd_d, x=ppt, accumulate = TRUE))%>%
  group_by(yr_h) %>%
  summarise(mcwd_c = min(cwd_c)) %>%ungroup()

###
min_hum <- ce6 %>% 
  rename(hum =  `(g/kg) Min Specific Humidity (CFSR) at 52.3905W,-13.0795N, 2003-01-01 to 2018-12-01`) %>%
  mutate(yr = year(DateTime),
         mo = month(DateTime),
         day = day(DateTime),
         yr_h = ifelse(mo>mm, yr+1, yr))%>%
  group_by(yr_h, mo, day) %>%
  summarise(hum = min(hum)) %>%ungroup() %>%
  mutate(HC = ifelse(hum > 10, 0,1),
         HCD = cons2(HC)*HC) %>%
  group_by(yr_h) %>%
  summarise(hum_c = max(HCD)) %>%ungroup()

######################################################################################################
#Merging climate data
######################################################################################################
clima <- max_temp %>% 
  left_join(max_vpd) %>%
  left_join(max_cwd) %>%
  left_join(max_mcwd) %>%
  left_join(max_mcwd_chirps) %>%
  left_join(min_hum) %>%
  rename(yr_f = yr_h) #%>%
#mutate_at(vars(MTC:hum_c), fun = scale)
#pairs((clima))
######################################################################################################

save(clima, file = "/Users/paulobrando/Dropbox/Scripts/Savanizacao/Clima_Engine/Climate_Engine_Tanguro.RData")
