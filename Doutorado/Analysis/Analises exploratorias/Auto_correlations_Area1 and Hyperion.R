library(GGally)

struc = df %>% 
  select('data','parcela','evi','ndvi','nbri','vari','vig','biomass','lai','litter','fuel')

ggpairs(struc)


bioc = df %>% 
  select('data','parcela','ari','lwvi2','msi','ndii','pssr','psri','sipi','wbi','biomass','lai','litter','fuel')

ggpairs(bioc)


phy = df %>% 
  select('data','parcela','pri','rendvi','biomass','lai','litter','fuel')

ggpairs(phy)



phy = df %>% 
  select('parcela','pri','rendvi','biomass','lai','litter','fuel')

ggpairs(phy, aes(color = parcela), lower = list(continuous = "smooth"), axisLabels = "none")

ggpairs(phy, aes(color = parcela), axisLabels = "none")




ggduo(phy, aes(color = parcela), types = list(continuous = "smooth"), axisLabels = "none")


ggcorr(phy)









