##Parte 2
#Introdução ao Ambiente Estatístico R
#Manipulação de banco de dados - 29/05/2019

#Instalando pacotes
#install.packages('dplyr')
#install.packages('ggplot2')

#Aqui temos duas planilhas uma CSV e uma TXT, nosso aobjetivo é exploralas e junta-las.
#Abrindo uma planilha de dados (.CSV e .TXT)
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Programas/R/Science-Repository/Courses/Introdução ao ambiente estatístico R/Material alunos")
dir()

txtobj <- read.table("DF_intro_R.txt", sep = "\t", header = TRUE)
txtobj


csvobj <- read.csv("DF_intro_R.csv", sep = ",", header = TRUE)
csvobj

#Visualizando a planilha
View(txtobj)
View(csvobj)

#Fazendo um sumario dos dados
summary(txtobj)
summary(csvobj)

#Calculando a media, maxima e minima de uma coluna especifica
mean(csvobj$pes)
max(csvobj$idd)
min(txtobj$alt)

#Unindo as planilhas
nomes = merge(txtobj, csvobj, by="nome")
nomes

#Alterando o titulo das colunas
colnames(nomes) = c("nome", "genero", "altura", "idade", "peso")
nomes

#Extraindo os dados de nome, altura e peso
library(dplyr)

nomes2 = select(nomes, nome, altura, peso)
nomes2

#Calculando o IMC(Indice de Massa Corporea) de cada pessoa
#29 Calcule o IMC dos personagens de Star Wars. (Dica: IMC = Massa em kg dividido pelo quadrado da altura em m)
imc_nomes = mutate(nomes2, imc = peso/(sqrt(altura)))
imc_nomes

#Plotando os dados e exportando os graficos
barplot(imc_nomes$imc)
barplot(imc_nomes$peso)
hist(imc_nomes$imc)
hist(imc_nomes$altura)
pie(imc_nomes$imc)

#Exportando o novo data frame
write.table(imc_nomes, "C:/Users/Eduardo Q Marques/Documents/My Jobs/Introdução ao ambiente estatístico R/Material alunos/imc.txt")
write.csv(imc_nomes, "C:/Users/Eduardo Q Marques/Documents/My Jobs/Introdução ao ambiente estatístico R/Material alunos/imc.csv")