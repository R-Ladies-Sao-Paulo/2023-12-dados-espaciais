# Filtrando para barragens do Município de Brumadinho, Minas Gerais
sigbm_mg <- sigbm |> 
  dplyr::filter(uf == "MG", municipio == "BRUMADINHO")

sigbm_mg |> 
  ggplot() +
  geom_sf()

brumadinho <- geobr::read_municipality(code_muni = 3109006) 

sigbm_mg |> 
  leaflet() |> 
  addProviderTiles("Esri.WorldImagery") |> 
  addPolygons(data = brumadinho, label = ~name_muni , fillOpacity = 0) |>
  addMarkers(~X, ~Y,
             clusterOptions = markerClusterOptions(),
             popup = ~ texto)

# definindo os limites da nossa área de interesse
lim <- sf::st_bbox(brumadinho)

# Vamos baixar os dados da Coleçao 8 do MapBiomas para a região do Muninícipio
# de Brumadinho (fonte: https://brasil.mapbiomas.org/en/colecoes-mapbiomas/)
# Explore os dados do MapBiomas https://plataforma.brasil.mapbiomas.org/

uso_da_terra <- terra::rast("brasil_coverage_2022.tif") |> 
  terra::crop(lim)

terra::plot(uso_da_terra)

classes_freq <- terra::freq(uso_da_terra)
dplyr::glimpse(classes_freq)
unique(classes_freq$value)

# Vamos mudar as cores de cada classe de uso da terra
legenda = read.csv("legenda-mapbiomas-8.csv", sep = ";", h = T)
dplyr::glimpse(legenda)

classes_freq = dplyr::left_join(classes_freq, legenda, 
              by = dplyr::join_by(value == Classe))

dplyr::glimpse(classes_freq)
colnames(classes_freq) <- c("Camada", "Classe", "Frequencia", "Descricao", "Cor")
cores <- classes_freq[, c("Classe", "Cor")]

terra::coltab(uso_da_terra) <- cores
terra::plot(uso_da_terra)

# O que predomina na nossa paisagem?
classes_freq |> 
  dplyr::arrange(-Frequencia) |> 
  head(5)

# Vamos filtrar esse dado para as camadas do nosso interesse.
# Como estamos lidando com barragens, vamos manter as classes
# de água, mineração e vegetação. 
View(classes_freq)
classes_de_interesse <- classes_freq |> dplyr::filter(Classe %in% 
                                                        c(3,4,11,12,30,33))
classes_de_interesse
classes_de_interesse$Classe

r = c(3,3,3,
      4,4,4,  
      5,10, NA,
      11,11,11,
      12,12, 12,
      13, 29, NA,
      30, 30, 30,
      31, 32, NA,
      33, 33, 33)
m = matrix(r, byrow = T, ncol = 3)
m

uso_da_terra <- terra::classify(uso_da_terra, m, include.lowest = T, right = F)
cores <- cores |> dplyr::filter(Classe %in% c(3,4,11,12, 30,33))
terra::coltab(uso_da_terra) <- cores
terra::plot(uso_da_terra)

# Vamos visualizar as barragens na paisagem classificada
proj <- uso_da_terra |> 
  sf::st_crs()
sf::st_crs(sigbm_mg) <- proj
sigbm_mg <- sf::st_transform(sigbm_mg, 4326)

plot(sigbm_mg$geometry,
     pch = 20, cex = 1.5, col = "hotpink",
     xlab = "long", ylab = "lat", add = T)

# Vamos observar a área atingida pelo rompimento da Barragem 1 da Mina do Feijão
# fonte: IBGE (https://agenciadenoticias.ibge.gov.br/agencia-noticias/2012-agencia-de-noticias/noticias/23808-novos-dados-geoespaciais-mostram-area-atingida-pelo-rompimento-da-barragem)
dir('Municipios_localidades_Afetados_MG/', pattern = ".shp")
rejeitos <- sf::st_read("Municipios_localidades_Afetados_MG/limites_rejeitos_dia29_v2.shp")

plot(rejeitos$geometry, col = "yellow", add = T)

# Agora vamos visualisar de forma interativa com o {mapview}
install.packages("mapview")
library(mapview)

barragem <- sigbm_mg |> 
  dplyr::filter(nome_da_barragem == "VI")

mapview(sigbm_mg$geometry, color="hotpink", col.regions = "hotpink") +
  mapview(barragem$geometry, color="red", col.regions = "red") +
  mapview(rejeitos, color="coral")

# Vamos avaliar o papel do relevo na definição 
# da área atingida
barragem <- sigbm_mg |> 
  dplyr::filter(nome_da_barragem == "VI")

lim2 <- sf::st_bbox(rejeitos) |> 
  sf::st_as_sfc() |> 
  sf::st_sf()

mapview(sigbm_mg$geometry, color="hotpink", col.regions = "hotpink") +
  mapview(barragem$geometry, color="red", col.regions = "red") +
  mapview(rejeitos, color="coral") +
  mapview(lim2, alpha.regions = 0)

install.packages("elevatr")
library(elevatr)
lim2 <- sf::st_transform(lim2, crs = 4326)
rejeitos <- sf::st_transform(rejeitos, crs = 4326)

elevation <- get_elev_raster(locations = lim2, 
    z = 14) |> 
  terra::rast() |> 
  terra::crop(rejeitos)
  
terra::plot(elevation)
plot(rejeitos$geometry, add = T)

elevation_df <- as.data.frame(elevation, xy = T) %>%
  na.omit()
head(elevation_df)
dim(elevation_df)
mean(elevation_df$file6fac32353291)

# Atenção: rejeitos é uma LINESTRING e deve ser um POLYGON
rejeitos <- rejeitos |> 
  sf::st_cast(to = "POLYGON")

plot(rejeitos$geometry, add = T)

rejeitos_rast <- terra::rasterize(rejeitos, elevation)

terra::plot(elevation)
terra::plot(rejeitos_rast, add = T)
plot(rejeitos$geometry, add = T)

# Agora sim podemos extrair os valores de elevação
elevation_rejeitos <- terra::extract(elevation, rejeitos)
head(elevation_rejeitos)
dim(elevation_rejeitos)
mean(elevation_rejeitos$file6fac32353291)

# E para finalizar, vamos baixar imagens de antes depois do rompimento
# e visualiza-las lado a lado. 

# AQUI EU VOU FAZER UMA ROTINA COM O RGEE PARA BAIXAR AS IMAGENS AI PENSEI
# QUE PODERÍAMOS TERMINAR COM UMA FIGURA 'PUBLICATION READY', O QUE ACHA?
install.packages("rgee")
library(rgee)
ee_install(py_env = "rgee")
ee_check()
reticulate::py_available()
ee_Initialize(drive = TRUE)
rgee::ee_check_credentials()

# Imagens landsat (fonte: https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC08_C02_T1_L2)
imagens <- ee$ImageCollection('LANDSAT/LC08/C02/T1_L2')$filterDate('2019-01-23', '2019-02-29')
ee_print(dataset)

# fim ------------------------------
