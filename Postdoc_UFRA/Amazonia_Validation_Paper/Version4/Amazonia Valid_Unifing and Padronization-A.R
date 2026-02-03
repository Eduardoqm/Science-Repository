#Amazonian Validations Proccess

#Eduardo Q Marques 02-05-2024

library(terra)

setwd("C:/Users/Workshop/Documents/Research/Postdoc_UFRA/Papers/Culturas_permanentes (Silverio et al)/Amazonia_validation")
dir()

#Separate names of classes -----------------------------------------------------
#am1 = vect("Amazonia_0_1000.shp")
#am2 = vect("Amazonia_1001_2000.shp")
#am3 = vect("Amazonia_2000_2591.shp")

#am1$Name2 = substring(am1$Name, 9, 40)
#am2$Name2 = substring(am2$Name, 8, 40)
#am3$Name2 = substring(am3$Name, 9, 40)

#writeVector(am1, "Amazonia_0_1000B.shp")
#writeVector(am2, "Amazonia_1001_2000B.shp")
#writeVector(am3, "Amazonia_2000_2591B.shp")

#Join Shapes -------------------------------------------------------------------
am1 = vect("Amazonia_0_1000B.shp")
am2 = vect("Amazonia_1001_2000B.shp")
am3 = vect("Amazonia_2000_2591B.shp")

unique(am1$Name2)
unique(am2$Name2)
unique(am3$Name2)

amx = rbind(am1, am2, am3)

#writeVector(amx, "Amazonia_full.shp")

#Patterning names --------------------------------------------------------------
unique(amx$Name2)
amx$Name3 = amx$Name2

#Correct classes
"formacao_savanica"
"formacao_campestre"
"indefinido"
"floresta_plantada"

#Correct names to reclass
"formacao_florestal"
"rio_lago_e_oceano"
"outra_formacao_nao_florestal"
"pastagem"
"cultura_anual_e_perene"
"outra_area_nao_vegetada"
"mangue"
"infraestrutura_humana"
"transicao"
"apicum"
"mineracao"
"praia"

amx$Name3[amx$Name3 == "1_formacao_florestal"] = "formacao_florestal"
amx$Name3[amx$Name3 == "_formacao_florestal"] = "formacao_florestal"
amx$Name3[amx$Name3 == "ormacao_florestal"] = "formacao_florestal"

amx$Name3[amx$Name3 =="rio_lagos_e_oceano"] = "rio_lago_e_oceano"
amx$Name3[amx$Name3 == "rio"] = "rio_lago_e_oceano"
amx$Name3[amx$Name3 == "lago"] = "rio_lago_e_oceano"                          
amx$Name3[amx$Name3 == "_rio"] = "rio_lago_e_oceano"
amx$Name3[amx$Name3 == "_lago"] = "rio_lago_e_oceano"
amx$Name3[amx$Name3 == "_oceano"] = "rio_lago_e_oceano"


amx$Name3[amx$Name3 == "outa_formacao_nao_florestal"] = "outra_formacao_nao_florestal"
amx$Name3[amx$Name3 == "outra_formacao_nao__florestal"] = "outra_formacao_nao_florestal"
amx$Name3[amx$Name3 == "outra_foramcao_nao_florestal"] = "outra_formacao_nao_florestal"
amx$Name3[amx$Name3 == "outra_formacao_nao_florstal"] = "outra_formacao_nao_florestal"
amx$Name3[amx$Name3 == "_outra_formacao_nao_florestal"] = "outra_formacao_nao_florestal"
amx$Name3[amx$Name3 == "formacao_nao_florestal"] = "outra_formacao_nao_florestal"
amx$Name3[amx$Name3 == "_outrra_formacao_nao_florestal"] = "outra_formacao_nao_florestal"
amx$Name3[amx$Name3 == "outra_area_nao_florestal"] = "outra_formacao_nao_florestal"


amx$Name3[amx$Name3 == "pastegem_cultivada"] = "pastagem"
amx$Name3[amx$Name3 == "pastagem_cutivada"] = "pastagem"
amx$Name3[amx$Name3 == "_pastagem_cultivada"] = "pastagem"
amx$Name3[amx$Name3 == "pastagem_cultivada"] = "pastagem"

amx$Name3[amx$Name3 == "cultura_anual"] = "cultura_anual_e_perene"

amx$Name3[amx$Name3 == "utra_area_nao_vegetada"] = "outra_area_nao_vegetada"
amx$Name3[amx$Name3 == "outra_formacao_nao_vegetada"] = "outra_area_nao_vegetada"
amx$Name3[amx$Name3 == "_outra_area_nao_vegetada"] = "outra_area_nao_vegetada"

amx$Name3[amx$Name3 == "_mangue"] = "mangue"

amx$Name3[amx$Name3 == "infraestrutura_urbana"] = "infraestrutura_humana"

amx$Name3[amx$Name3 == "ransicao"] = "transicao"                      
amx$Name3[amx$Name3 == "borda_formacao_florestal"] = "transicao"

amx$Name3[amx$Name3 == "_apicum"] = "apicum"

amx$Name3[amx$Name3 == "_mineracao"] = "mineracao"             

amx$Name3[amx$Name3 == "_praia"] = "praia"

unique(amx$Name2)
unique(amx$Name3)

writeVector(amx, "Amazonia_full.shp")
                      

                 
 








