#Driest Period - Nathalia Rasters
#Eduardo Q Marques 18-10-2026

library(terra)

setwd("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Rasters/Driest_Period_Nathalia")
dir()

dp_start = rast("DP_onset_cy.tif")
dp_end = rast("DP_end_cy.tif")
wet_peak = rast("wet_peak_cy.tif")

plot(dp_start, main = "DP Start")
plot(dp_end, main = "DP End")
plot(wet_peak, main = "Wet Peak")

am = vect("G:/My Drive/Geodata/Vectors/Amazonia.shp")
plot(am, add = T)

#Calendar convertion to hidrologic year ----------------------------------------
dp_start_hy = ((dp_start - wet_peak + 73) %% 73) + 1
dp_end_hy   = ((dp_end - wet_peak + 73) %% 73) + 1

plot(dp_start_hy)
plot(dp_end_hy)

#Number of Dry months ----------------------------------------------------------
dp_length_pentads = dp_end_hy - dp_start_hy + 1
plot(dp_length_pentads)

dp_length_pentads = mask(crop(dp_length_pentads, am), am)
plot(dp_length_pentads)

dp_length_days = dp_length_pentads * 5 # Pentad ~5 days
plot(dp_length_days)

dp_length_months = dp_length_days / (365 / 12) # One month = 365 / 12
plot(dp_length_months)

dp_months = round(dp_length_months)
plot(dp_months)

#Exporting rasters -------------------------------------------------------------
#writeRaster(dp_start_hy, "DP_onset_hy.tif", overwrite = TRUE)
#writeRaster(dp_end_hy, "DP_end_hy.tif", overwrite = TRUE )
#writeRaster(dp_months, "DP_length_months.tif", overwrite = TRUE )

#Clustering Dry duration -------------------------------------------------------
rcl_matrix = matrix(c(
  0, 3,  1,   # Grupo 1: Seca Curta (0 a 3 meses)
  3, 6,  2,   # Grupo 2: Seca Moderada (3 a 6 meses)
  6, 8,  3,   # Grupo 3: Seca Longa (6 a 9 meses)
  8, 12, 4),    # Grupo 4: Seca Muito Longa (9 a 12 meses)
  ncol = 3, byrow = TRUE)

dp_zones_4g = classify(dp_months, rcl_matrix, include.lowest = TRUE)
dp_zones_4g = as.factor(dp_zones_4g)

levels(dp_zones_4g) = data.frame(id = 1:4, 
  zone = c("Short (0-3m)", "Moderate (3-6m)", "Long (6-8m)", "Extra Long (8-12m)"))

plot(dp_zones_4g, main = "Dry Season Pattern (Nathalia data)",
     col = c("blue", "lightgreen", "orange", "red"))

#k-means não supervisionado (duração + início + fim) ---------------------------
# Máscara para a Amazônia
dp_length_months_am = mask(crop(dp_length_months, am), am)
dp_start_hy_am      = mask(crop(dp_start_hy,      am), am)
dp_end_hy_am        = mask(crop(dp_end_hy,        am), am)

plot(dp_length_months_am, main = "DP length (months, contínuo)")
plot(am, add = TRUE)

# Stack de features contínuas (padronizar é essencial: escalas diferentes)
feat = c(dp_length_months_am, dp_start_hy_am, dp_end_hy_am)
names(feat) = c("length", "start_hy", "end_hy")

set.seed(42)
samp = spatSample(feat, size = 5000, method = "random", na.rm = TRUE, as.df = TRUE)

# Padronizar antes do k-means
samp_sc = scale(samp)
km = kmeans(samp_sc, centers = 4, nstart = 25, iter.max = 50)

# Prever para o raster inteiro: padronizar o raster com os mesmos parâmetros
# (médias e desvios da amostra)
mu = attr(samp_sc, "scaled:center")
sd = attr(samp_sc, "scaled:scale")

feat_sc = (feat - mu[1]) / sd[1]  # placeholder; ver função abaixo
# Função para padronizar cada camada com seu próprio mu/sd:
standardize_layer = function(r, m, s) (r - m) / s
feat_sc = c(
  standardize_layer(feat[[1]], mu["length"],   sd["length"]),
  standardize_layer(feat[[2]], mu["start_hy"], sd["start_hy"]),
  standardize_layer(feat[[3]], mu["end_hy"],   sd["end_hy"])
)
names(feat_sc) = names(feat)

# Predição pixel a pixel com os centróides do kmeans
# (distância euclidiana ao centróide mais próximo)
cent = km$centers  # matriz k x 3
k = nrow(cent)

# Empilhar distâncias a cada centróide e pegar o argmin
dist_stack = rast(lapply(1:k, function(i) {
  (feat_sc[[1]] - cent[i,1])^2 +
    (feat_sc[[2]] - cent[i,2])^2 +
    (feat_sc[[3]] - cent[i,3])^2
}))
km_class = which.min(dist_stack)  # índice do centróide mais próximo
plot(km_class, main = paste0("k-means (k=", k, ") features: length+start+end"))

# Limpando o ruído do k-means (janela 5x5)
km_class_clean = focal(km_class, w=3, fun="median", na.policy="omit")
plot(km_class_clean, main="k-means k=3 (Smoothed)")

# Tabela: quantos pixels por cluster e média das features originais
km_df = as.data.frame(c(feat, km_class_clean), na.rm = TRUE)
names(km_df)[4] = "cluster"
agg = aggregate(km_df[,1:3], by = list(cluster = km_df$cluster), FUN = mean)
print(agg)















































