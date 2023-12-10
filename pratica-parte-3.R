
# Vamos avaliar o papel do relevo na definição 
# da área atingida
lim2 <- sf::st_bbox(rejeitos) |> 
  sf::st_as_sfc() |> 
  sf::st_sf()

mapview(sigbm_brumadinho$geometry, color="hotpink", col.regions = "hotpink") +
  mapview(barragem_vi$geometry, color="red", col.regions = "red") +
  mapview(rejeitos, color="coral") +
  mapview(lim2, alpha.regions = 0)

# install.packages("elevatr")
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

barragem_utm <- sf::st_transform(barragem_vi, terra::crs(img_depois))
rejeitos_utm <- sf::st_transform(rejeitos, terra::crs(img_depois))
plot(rejeitos_utm$geometry, add = T)
plot(barragem_utm$geometry,
     pch = 10, cex = 1.5, col = "red",add = T)

# fim ------------------------------
