#Driest Period - Nathalia Rasters
#Eduardo Q Marques 30-09-2026

library(terra)

setwd("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Rasters/Driest_Period_Nathalia")
dir()

dp_start = rast("DP_onset_cy.tif")
dp_end = rast("DP_end_cy.tif")
wet_peak <- rast("wet_peak_cy.tif")

plot(dp_start)
plot(dp_end)
plot(wet_peak)

#Test 1 ------------------------------------------------------------------------
#Calendar convertion to hidrologic year
dp_start_hy <- ((dp_start - wet_peak + 73) %% 73) + 1
dp_end_hy   <- ((dp_end - wet_peak + 73) %% 73) + 1

plot(dp_start_hy)
plot(dp_end_hy)

#Test 2 ------------------------------------------------------------------------
# Converter pentads em dias aproximados do ano
pentad_to_day <- function(x) {
  (x - 1) * 5 + 1
}

start_day <- pentad_to_day(dp_start)
end_day   <- pentad_to_day(dp_end)

# Converter dias em meses (ano não bissexto)
start_month <- app(start_day, function(x) {
  as.integer(format(
    as.Date(x - 1, origin = "2021-01-01"),
    "%m"
  ))
})

end_month <- app(end_day, function(x) {
  as.integer(format(
    as.Date(x - 1, origin = "2021-01-01"),
    "%m"
  ))
})

names(start_month) <- "DP_start_month"
names(end_month)   <- "DP_end_month"

plot(start_month, main = "DP onset month")
plot(end_month, main = "DP end month")