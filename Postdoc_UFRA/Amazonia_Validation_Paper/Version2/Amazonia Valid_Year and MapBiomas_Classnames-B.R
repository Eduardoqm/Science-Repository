#Amazonian Validations Process

#Extract year and change names of class to MapBiomas

#Eduardo Q Marques 04-07-2024 updated: 20-08-2024

library(terra)

setwd("G:/My Drive/Postdoc_UFRA/Papers/Amazonia_validation (Marques et al)/Shapes")
dir()

am = vect("Amazonia_full_B.shp")
plot(am)

am_hansen = vect("hansen_500_points.shp")
plot(am_hansen, col = "red", add = T)

#Extract year information ------------------------------------------------------
am$year1 = substr(am$Name, 4, 7)

#Corrections
am$year1[am$year1 == "0201"] = NA
am$year1[am$year1 == "204_"] = "2005"
am$year1[am$year1 == "202_"] = "2002"

am$year1 = as.numeric(am$year1)
unique(am$year1)

am$year2 = substr(am$Name, 3, 6)
am$year2 = as.numeric(am$year2)
am$year2[am$year2 < 1980] = NA
unique(am$year2)

#Join Dates --------------------------------------------------------------------
am$year1[is.na(am$year1)] = 0
am$year2[is.na(am$year2)] = 0
am$year = (am$year1 + am$year2)
unique(am$year)

#am = am[,c(6,1,2,3)]
am = am[,c(6,3)]
head(data.frame(am))

#Join Hansen points ------------------------------------------------------------
am_hansen$year = 2024
am_hansen$Name3 = am_hansen$Name2
am_hansen = am_hansen[,c(3,4)]
head(data.frame(am_hansen))

am = rbind(am, am_hansen)

am$year[am$year == 0] <- 2022
View(data.frame(am))

#Patterning names to MapBiomas -------------------------------------------------
am$Name3[is.na(am$Name3)] = c("indefinido")
unique(am$Name3)
am$Name_MB = am$Name3
am$MB_id = am$Name3
am$MB_color = am$Name3

#MapBiomas Classes names
am$Name_MB[am$Name_MB == "formacao_florestal"] = c("Forest Formation")
am$Name_MB[am$Name_MB == "formacao_savanica"] = c("Savanna Formation")
am$Name_MB[am$Name_MB == "rio_lago_e_oceano"] = c("River, Lake and Ocean")
am$Name_MB[am$Name_MB == "pastagem"] = c("Pasture")
am$Name_MB[am$Name_MB == "transicao"] = c("Transition")
am$Name_MB[am$Name_MB == "formacao_campestre"] = c("Grassland")
am$Name_MB[am$Name_MB == "cultura_anual"] = c("Temporary Crop")
am$Name_MB[am$Name_MB == "cultura_perene"] = c("Perennial Crop")
am$Name_MB[am$Name_MB == "indefinido"] = c("Not Observed")
am$Name_MB[am$Name_MB == "infraestrutura_humana"] = c("Urban Area")
am$Name_MB[am$Name_MB == "outra_area_nao_vegetada"] = c("Other non Vegetated Areas")
am$Name_MB[am$Name_MB == "outra_formacao_nao_florestal"] = c("Other non Forest Formations")
am$Name_MB[am$Name_MB == "mineracao"] = c("Mining")
am$Name_MB[am$Name_MB == "mangue"] = c("Mangrove")
am$Name_MB[am$Name_MB == "floresta_plantada"] = c("Forest Plantation")
am$Name_MB[am$Name_MB == "praia"] = c("Beach, Dune and Sand Spot")
am$Name_MB[am$Name_MB == "formacao_nao_florestal"] = c("Other non Forest Formations")

#MapBiomas Classes ID
am$MB_id[am$MB_id == "formacao_florestal"] = c("3")
am$MB_id[am$MB_id == "formacao_savanica"] = c("4")
am$MB_id[am$MB_id == "rio_lago_e_oceano"] = c("33")
am$MB_id[am$MB_id == "pastagem"] = c("15")
am$MB_id[am$MB_id == "transicao"] = c("100")
am$MB_id[am$MB_id == "formacao_campestre"] = c("12")
am$MB_id[am$MB_id == "cultura_anual"] = c("19")
am$MB_id[am$MB_id == "cultura_perene"] = c("36")
am$MB_id[am$MB_id == "indefinido"] = c("27")
am$MB_id[am$MB_id == "infraestrutura_humana"] = c("24")
am$MB_id[am$MB_id == "outra_area_nao_vegetada"] = c("25")
am$MB_id[am$MB_id == "outra_formacao_nao_florestal"] = c("13")
am$MB_id[am$MB_id == "mineracao"] = c("30")
am$MB_id[am$MB_id == "mangue"] = c("5")
am$MB_id[am$MB_id == "floresta_plantada"] = c("9")
am$MB_id[am$MB_id == "praia"] = c("23")
am$MB_id[am$MB_id == "formacao_nao_florestal"] = c("13")

#MapBiomas Classes Colours
am$MB_color[am$MB_color == "formacao_florestal"] = c("#1f8d49")
am$MB_color[am$MB_color == "formacao_savanica"] = c("#7dc975")
am$MB_color[am$MB_color == "rio_lago_e_oceano"] = c("#2532e4")
am$MB_color[am$MB_color == "pastagem"] = c("#edde8e")
am$MB_color[am$MB_color == "transicao"] = c("orange")
am$MB_color[am$MB_color == "formacao_campestre"] = c("#d6bc74")
am$MB_color[am$MB_color == "cultura_anual"] = c("#C27BA0")
am$MB_color[am$MB_color == "cultura_perene"] = c("#d082de")
am$MB_color[am$MB_color == "indefinido"] = c("#ffffff")
am$MB_color[am$MB_color == "infraestrutura_humana"] = c("#d4271e")
am$MB_color[am$MB_color == "outra_area_nao_vegetada"] = c("#db4d4f")
am$MB_color[am$MB_color == "outra_formacao_nao_florestal"] = c("#d89f5c")
am$MB_color[am$MB_color == "mineracao"] = c("#9c0027")
am$MB_color[am$MB_color == "mangue"] = c("#04381d")
am$MB_color[am$MB_color == "floresta_plantada"] = c("#7a5900")
am$MB_color[am$MB_color == "praia"] = c("#ffa07a")
am$MB_color[am$MB_color == "formacao_nao_florestal"] = c("#d89f5c")

View(as.data.frame(am))

#Save new Shape ----------------------------------------------------------------
am_sf = sf::st_as_sf(am)
sf::st_write(am_sf, "Amazonia_full_V2.shp")

#Testing Plots -----------------------------------------------------------------
library(ggplot2)

ggplot(am_sf)+
  geom_sf(aes(col = Name_MB, fill = Name_MB))+
  #scale_color_manual(values = am_sf$MB_color)+
  #scale_fill_manual(values = am_sf$MB_color)+
  theme_minimal()
  










                      

                 
 