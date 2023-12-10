# Carregando pacotes
library(ggplot2)
library(leaflet)
# Importando a base do sigbm tratada
sigbm <- readr::read_rds("dados-output/sigbm.rds")


# Filtrando para barragens do Município de Brumadinho, Minas Gerais

sigbm_brumadinho <- sigbm |>
  dplyr::filter(uf == "MG", municipio == "BRUMADINHO")

brumadinho <- geobr::read_municipality(code_muni = 3109006)

sigbm_brumadinho |>
  ggplot() +
  geom_sf(data = brumadinho) +
  geom_sf() +
  theme_void()



sigbm_brumadinho |>
  leaflet() |>
  addProviderTiles("Esri.WorldImagery") |>
  addPolygons(data = brumadinho, label = ~name_muni, fillOpacity = 0) |>
  addMarkers(
    clusterOptions = markerClusterOptions(),
    popup = ~texto
  )

# definindo os limites da nossa área de interesse
lim <- sf::st_bbox(brumadinho)

# Vamos baixar os dados da Coleçao 8 do MapBiomas para a região do Muninícipio
# de Brumadinho (fonte: https://brasil.mapbiomas.org/en/colecoes-mapbiomas/)
# Explore os dados do MapBiomas https://plataforma.brasil.mapbiomas.org/


# Link direto:
# https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_8/lclu/coverage/brasil_coverage_2022.tif
if (!file.exists("dados/brasil_coverage_2022.tif")) {
  curl::multi_download(
    "https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_8/lclu/coverage/brasil_coverage_2022.tif",
    "dados/brasil_coverage_2022.tif",
    resume = TRUE
  )
}


# Importar a imagem de satélite usando a função rast() do pacote {terra}
uso_da_terra <- terra::rast("dados/brasil_coverage_2022.tif") |>
  # Recortar a imagem para a área de interesse
  terra::crop(lim)

# A classe desse objeto é Spatial Raster
class(uso_da_terra)

# Vamos visualizar a imagem de satélite, recortada para a imagem de interesse
terra::plot(uso_da_terra)

# A base brasil_coverage_2022 apresenta usos da terra
classes_freq <- terra::freq(uso_da_terra)

# Espiando a base
classes_freq

# Quais são as classes de uso da terra?
unique(classes_freq$value)

# Vamos mudar as cores de cada classe de uso da terra- ----

# Importando a legenda
legenda <- readr::read_csv2("dados/legenda-mapbiomas-8.csv")

dplyr::glimpse(legenda)

# Unindo as tabelas de frequência e legenda
classes_freq_legenda <- dplyr::left_join(classes_freq, legenda,
  by = dplyr::join_by(value == Classe)
)

dplyr::glimpse(classes_freq_legenda)

# Renomeando as colunas
colnames(classes_freq_legenda) <- c("Camada", "Classe", "Frequencia", "Descricao", "Cor")

# Buscando as cores
cores <- classes_freq_legenda |>
  dplyr::select(
    Classe, Cor
  )


# A função coltab() do pacote {terra} permite alterar as cores de uma imagem
terra::coltab(uso_da_terra) <- cores

# Ver o resultado
terra::plot(uso_da_terra)

# O que predomina na nossa paisagem?
classes_freq_legenda |>
  dplyr::arrange(-Frequencia) |>
  head(5)

# Vamos filtrar esse dado para as camadas do nosso interesse.
# Como estamos lidando com barragens, vamos manter as classes
# de água, mineração e vegetação.
View(classes_freq_legenda)

# Filtrando classes de interesse
classes_de_interesse <- classes_freq_legenda |>
  dplyr::filter(Classe %in%
    c(3, 4, 11, 12, 30, 33))


classes_de_interesse
classes_de_interesse$Classe


# Dúvida bea: O que essa matriz faz?
r <- c(
  3, 3, 3,
  4, 4, 4,
  5, 10, NA,
  11, 11, 11,
  12, 12, 12,
  13, 29, NA,
  30, 30, 30,
  31, 32, NA,
  33, 33, 33
)
matriz <- matrix(r, byrow = T, ncol = 3)
matriz



uso_da_terra_classificado <- terra::classify(uso_da_terra, matriz, include.lowest = T, right = F)

# Filtrando as cores de interesse
cores <- cores |> dplyr::filter(Classe %in% c(3, 4, 11, 12, 30, 33))

# Alterando as cores
terra::coltab(uso_da_terra_classificado) <- cores

# Plotando o mapa
terra::plot(uso_da_terra_classificado)

# Vamos visualizar as barragens na paisagem classificada

# Buscando o CRS do objeto uso_da_terra_classificado: é o WGS84
proj <- uso_da_terra_classificado |>
  sf::st_crs()

# Usando a projeção encontrada para transformar a base sigbm_brumadinho
sf::st_crs(sigbm_brumadinho) <- proj

# Outra forma é transformando com a função st_transform()
sigbm_brumadinho <- sf::st_transform(sigbm_brumadinho, 4326)

# Podemos visualizar a localização das barragens na paisagem
plot(sigbm_brumadinho$geometry,
  pch = 20, cex = 1.5, col = "hotpink",
  xlab = "long", ylab = "lat", add = T
)

# Vamos observar a área atingida pelo rompimento da Barragem 1 da Mina do Feijão
# fonte: IBGE (https://agenciadenoticias.ibge.gov.br/agencia-noticias/2012-agencia-de-noticias/noticias/23808-novos-dados-geoespaciais-mostram-area-atingida-pelo-rompimento-da-barragem)
dir("dados/Municipios_localidades_Afetados_MG/", pattern = ".shp")

rejeitos <- sf::st_read("dados/Municipios_localidades_Afetados_MG/limites_rejeitos_dia29_v2.shp")

plot(rejeitos$geometry, col = "yellow", add = T)

# Agora vamos visualisar de forma interativa com o {mapview}
library(mapview)
img <- "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a6/Brumadinho%2C_Minas_Gerais_%2847021723582%29.jpg/640px-Brumadinho%2C_Minas_Gerais_%2847021723582%29.jpg"

barragem_vi <- sigbm_brumadinho |>
  dplyr::filter(nome_da_barragem == "VI")

mapview(sigbm_brumadinho, color = "hotpink", col.regions = "hotpink") +
  mapview(barragem_vi,
    color = "red", col.regions = "red",
    popup = leafpop::popupTable(barragem, # popup é uma função do pacote {leafpop}
      zcol = c("texto")
    )
  ) + # aprenda mais em: https://r-spatial.github.io/mapview/articles/mapview_04-popups.html#image-popups
  mapview(rejeitos,
    color = "coral",
    popup = leafpop::popupImage(img, src = "remote")
  )
