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
img <- "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a6/Brumadinho%2C_Minas_Gerais_%2847021723582%29.jpg/640px-Brumadinho%2C_Minas_Gerais_%2847021723582%29.jpg"

barragem <- sigbm_mg |> 
  dplyr::filter(nome_da_barragem == "VI")

mapview(sigbm_mg, color="hotpink", col.regions = "hotpink") +
  mapview(barragem, color="red", col.regions = "red",
          popup = leafpop::popupTable(barragem,       # popup é uma função do pacote {leafpop}
                             zcol = c("texto"))) +    # aprenda mais em: https://r-spatial.github.io/mapview/articles/mapview_04-popups.html#image-popups
  mapview(rejeitos, color="coral", 
          popup = leafpop::popupImage(img, src = "remote"))

# Vamos avaliar o papel do relevo na definição 
# da área atingida
lim2 <- sf::st_bbox(rejeitos) |> 
  sf::st_as_sfc() |> 
  sf::st_sf()

mapview(sigbm_mg$geometry, color="hotpink", col.regions = "hotpink") +
  mapview(barragem$geometry, color="red", col.regions = "red") +
  mapview(rejeitos, color="coral") +
  mapview(lim2, alpha.regions = 0)

install.packages("elevatr")
library(elevatr)
lim2 <- sf::st_transform(lim2, crs = 4326)  # reprojetando o dado vetorial
rejeitos <- sf::st_transform(rejeitos, crs = 4326)

elevation <- get_elev_raster(locations = lim2, 
    z = 14) |> 
  terra::rast() |> 
  terra::crop(rejeitos)
  
terra::plot(elevation)
plot(rejeitos$geometry, add = T)

# Vamos examinar o papel da altitude na definição da área atingida
# pelo rompimento da barragem

# Primeiro vamos reprojetar os dados para utm
wkt_utm <- "PROJCRS[\"WGS 84 / UTM zone 23N\",\n    BASEGEOGCRS[\"WGS 84\",\n        ENSEMBLE[\"World Geodetic System 1984 ensemble\",\n            MEMBER[\"World Geodetic System 1984 (Transit)\"],\n            MEMBER[\"World Geodetic System 1984 (G730)\"],\n            MEMBER[\"World Geodetic System 1984 (G873)\"],\n            MEMBER[\"World Geodetic System 1984 (G1150)\"],\n            MEMBER[\"World Geodetic System 1984 (G1674)\"],\n            MEMBER[\"World Geodetic System 1984 (G1762)\"],\n            MEMBER[\"World Geodetic System 1984 (G2139)\"],\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]],\n            ENSEMBLEACCURACY[2.0]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4326]],\n    CONVERSION[\"UTM zone 23N\",\n        METHOD[\"Transverse Mercator\",\n            ID[\"EPSG\",9807]],\n        PARAMETER[\"Latitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8801]],\n        PARAMETER[\"Longitude of natural origin\",-45,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"Scale factor at natural origin\",0.9996,\n            SCALEUNIT[\"unity\",1],\n            ID[\"EPSG\",8805]],\n        PARAMETER[\"False easting\",500000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Engineering survey, topographic mapping.\"],\n        AREA[\"Between 48°W and 42°W, northern hemisphere between equator and 84°N, onshore and offshore. Greenland.\"],\n        BBOX[0,-48,84,-42]],\n    ID[\"EPSG\",32623]]"
sf::st_crs(elevation_df) <- wkt_utm
rejeitos <- sf::st_transform(rejeitos, terra::crs(wkt_utm)) # vetor
elevation <- terra::project(elevation, terra::crs(wkt_utm)) # raster

rejeitos_buffer <- sf::st_buffer(rejeitos, 100)

terra::plot(elevation)
plot(rejeitos$geometry, add = T)
plot(rejeitos_buffer$geometry, add = T)

# Vamos obter os valores de altitudes da região afetada
elevation_df <- as.data.frame(elevation, xy = T) |> 
  na.omit()

head(elevation_df)

# Atenção: rejeitos é uma LINESTRING e deve ser um POLYGON
rejeitos
rejeitos <- rejeitos |> 
  sf::st_cast(to = "POLYGON")

rejeitos_rast <- terra::rasterize(rejeitos, elevation)
terra::plot(elevation)
terra::plot(rejeitos_rast, add = T)

# Agora que nossa área de rejeitos está representada por um polígono,
# podemos extrair os valores de elevação da região atingida.

elevation_rejeitos <- terra::extract(elevation, rejeitos)
head(elevation_rejeitos)

# Note que usando a função terra::extract() não temos os valores
# das coordenadas das células.

# Vamos obter as coordenadas dessas células:
tmp <- as.data.frame(rejeitos_rast, xy = T)|> 
  sf::st_as_sf(coords = c("x", "y"))

dim(tmp)
dim(elevation_rejeitos) # Note que 61 células foram perdidas na transformação de vetor para raster

# Aqui transformamos nossa tabela de valores de elevação em um
# vetor sf e adicionamos duas colunas em sua tabela de atributos:
# Elevation - Coluna com valores de altitude
# Atingido - Coluna binária onde 0 representa não atingida e 1, atingida.
elevation_df <- elevation_df |>
  sf::st_as_sf(coords = c("x", "y")) |> 
  dplyr::mutate(Elevation = elevation_df$file51381c156d4b,
               Atingido = 0) |> 
  dplyr::select(Elevation, Atingido)

sf::st_crs(elevation_df) <- terra::crs(elevation)
elevation_df <- sf::st_transform(elevation_df, terra::crs(elevation))

plot(elevation_df$geometry, add = T)

# Vamos obter a id (posição da linha) das células atingidas em dois passos:
# filtrando a tabela para as celulas atingidas e
# ficando só com a informação de id da célula
id_atingidas <- elevation_df[which(elevation_df$geometry %in% tmp$geometry), ]
id_atingidas <- rownames(id_atingidas) |> as.numeric()

# Agora vamos inserir o valor um nas linhas das células atingidas.
elevation_df[id_atingidas, "Atingido"]$Atingido <- 1
unique(elevation_df$Atingido) # deve ser 0 e 1


# # E para finalizar, vamos baixar imagens de antes depois do rompimento
# # e entender como lidamos com imagens matriciais com mais de uma camada.
# 
# # Imagens Landsat 8 (fonte: https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC08_C02_T1_L2)
# img_antes <- terra::rast("Antes.tif")
# img_depois <- terra::rast("Depois.tif")
# 
# # Band 1 (ultra blue, coastal aerosol) surface reflectance
# # Band 2 (blue) surface reflectance
# # Band 3 (green) surface reflectance
# # Band 4 (red) surface reflectance
# # Band 5 (near infrared) surface reflectance
# # Band 6 (shortwave infrared 1) surface reflectance
# # Band 7 (shortwave infrared 2) surface reflectance
# 
# terra::plot(img_antes)
# 
# terra::plotRGB(img_antes, r = 4, g = 3, b = 2, 
#                scale = 255, stretch = "lin")
# terra::plotRGB(img_depois, r = 4, g = 3, b = 2, 
#                scale = 255, stretch = "lin")
# 
# viewRGB(raster::stack(img_antes), 4, 3, 2, method = "ngb",
#         quantiles = c(0, 1),
#         maxpixels = raster::ncell(img_antes)) + 
#   viewRGB(raster::stack(img_depois), 4, 3, 2, method = "ngb",
#           quantiles = c(0, 1),
#           maxpixels = raster::ncell(img_depois)) 
# 
# terra::plot(img_antes[[5]], col = gray(0:100 / 100))
# terra::plot(img_depois[[5]], col = gray(0:100 / 100))
# 
# # NDVI = (NIR-Vermelho) / (NIR+Vermelho)
# NDVI_antes <- (img_antes[[5]] - img_antes[[4]]) / (img_antes[[5]] + img_antes[[4]])
# NDVI_depois <- (img_depois[[5]] - img_depois[[4]]) / (img_depois[[5]] + img_depois[[4]])
# 
# terra::plot(NDVI_antes)
# terra::plot(NDVI_depois)

barragem_utm <- sf::st_transform(barragem, terra::crs(img_depois))
rejeitos_utm <- sf::st_transform(rejeitos, terra::crs(img_depois))
plot(rejeitos_utm$geometry, add = T)
plot(barragem_utm$geometry,
     pch = 10, cex = 1.5, col = "red",add = T)

# fim ------------------------------
