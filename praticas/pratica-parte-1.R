# Obteção dos dados ---------
# Download dos dados de barragens de mineração no Brasil
# https://app.anm.gov.br/SIGBM/Publico/ClassificacaoNacionalDaBarragem

# Conseguimos “imitar” o processo de baixar o arquivo usando programação.

# Salvando o link em um objeto
link_sigbm <-
  "https://app.anm.gov.br/SIGBM/Publico/ClassificacaoNacionalDaBarragem/ExportarExcel"

# Fazendo uma requisição POST neste link,
# e salvando o arquivo localmente
httr::POST(
  link_sigbm,
  httr::write_disk("dados/sigbm.xlsx", overwrite = TRUE)
)

# Importação dos dados ------------

# Importar dados, pulando 4 linhas iniciais
sigbm_bruto <- readxl::read_excel("dados/sigbm.xlsx", skip = 4)

# Ver quais colunas a base apresenta
dplyr::glimpse(sigbm_bruto)

# Limpeza dos dados

sigbm <- sigbm_bruto |>
  # Limpando nome das colunas!
  janitor::clean_names() |>
  dplyr::mutate(
    # Arrumando as colunas de lat/long
    lat = parzer::parse_lat(latitude),
    long = parzer::parse_lon(longitude),
    # Adiciona uma coluna com o nome da barragem e empreendedor
    texto = glue::glue(
      "Nome da barragem: {nome_da_barragem} <br>
      Empreendedor: {nome_do_empreendedor}"
    )
  ) |>
  # Removendo linhas onde lat/long é igual a 0 (erro de cadastro)
  dplyr::filter(lat != 0, long != 0) |>
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

# Site útil para consultar o CRS
# https://epsg.io/

# Verificando a classe
class(sigbm)

# Novas informações:
head(sigbm)
# Geometry type
# Dimension
# Bounding box
# CRS

# Verificando o CRS
sf::st_crs(sigbm)


# Visualização estática -------------------------

library(ggplot2)

sigbm |>
  ggplot() +
  geom_sf()

# Vamos para a visualização interativa, e depois voltamos

# Visualização interativa ------------------------

# Quando é bom?
# Comunicação com o público: dashboards, apresentações
# Para explorar os dados!

# Não é bom: trabalhos científicos, artigos, relatórios - precisa ser estático


# Vamos fazer uma primeira visualização?
library(leaflet)

# Vamos criar o mapa de forma incremental!

# Versão 1: mapa vazio

sigbm |>
  # Começa um mapa vazio
  leaflet()

# Versão 2: vamos adicionar as barragens
sigbm |>
  leaflet() |>
  # Adiciona as barragens
  addMarkers()

# Versão 3: vamos adicionar um fundo de mapa
sigbm |>
  leaflet() |>
  addProviderTiles("Esri.WorldImagery") |>
  addMarkers()

# Versão 4: vamos agrupar os pontos

sigbm |>
  leaflet() |>
  addProviderTiles("Esri.WorldImagery") |>
  addMarkers(clusterOptions = markerClusterOptions())

# Versão 5: vamos adicionar um popup

sigbm |>
  leaflet() |>
  addProviderTiles("Esri.WorldImagery") |>
  addMarkers(
    clusterOptions = markerClusterOptions(),
    popup = ~texto
  )

# Vamos adicionar a delimitação dos estados brasileiros?

# Baixando os dados

estados <- geobr::read_state()


# Voltando ao mapa: adicionando os estados!

sigbm |>
  leaflet() |>
  addProviderTiles("Esri.WorldImagery") |>
  # Adicionando os estados
  addPolygons(data = estados, label = ~abbrev_state, fillOpacity = 0) |>
  addMarkers(
    clusterOptions = markerClusterOptions(),
    popup = ~texto
  )


# Vamos exportar a base do sigbm já tratada
readr::write_rds(sigbm, "dados-output/sigbm.rds")

# Até agora usamos:
# pacote parzer para limpar lat/long
# pacote leaflet para visualização interativa
# pacote geobr para baixar dados vetoriais do Brasil
# ainda precisamos trabalhar com dados raster/matriciais
# e também criar uma visualização estática mais bonita!
# Vamos para a parte 2 da prática :)
