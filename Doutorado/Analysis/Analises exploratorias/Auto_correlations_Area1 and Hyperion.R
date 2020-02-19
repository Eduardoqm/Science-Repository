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