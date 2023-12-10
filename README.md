# 2023-12-dados-espaciais

**Meetup:** Trabalhando com dados espaciais no R

**Data:** 12/12/2023

**Horário:** 19h

**Local:** Online

**Inscrições:** <https://www.meetup.com/pt-BR/rladies-sao-paulo/>

## Palestrantes

-   **Laís Brasileiro**

    -   Email: [laisbioufc\@gmail.com](mailto:laisbioufc@gmail.com)

-   **Beatriz Milz**

    -   Email: [milz.bea\@gmail.com](mailto:milz.bea@gmail.com)
    -   Website: <https://beamilz.com>


## Pacotes necessários

``` r
pacotes_necessarios <- c("httr",  "readxl", "janitor", "dplyr", "parzer", "glue",
"sf", "ggplot2", "leaflet", "geobr", "terra", "mapivew")

install.packages("pacotes_necessarios")
```

Caso tenha dificuldade, nos avise na página do meetup!


## Preparação para a prática

- Instale os pacotes necessários (veja acima)

- Faça download do arquivo `.zip` com os arquivos do curso: <https://github.com/R-Ladies-Sao-Paulo/2023-12-dados-espaciais/archive/refs/heads/main.zip>

- Descompacte o arquivo `.zip` em uma pasta de sua preferência

- Abra o arquivo do projeto `2023-12-dados-espaciais.Rproj` no RStudio

- Copie e cole o seguinte código no console. O objetivo é fazer download do arquivo `brasil_coverage_2022.tif` para a pasta `dados` do projeto. Esse arquivo é grande, então pode demorar um pouco para baixar.

```{r}
if (!file.exists("dados/brasil_coverage_2022.tif")) {
  curl::multi_download(
    "https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_8/lclu/coverage/brasil_coverage_2022.tif",
    "dados/brasil_coverage_2022.tif",
    resume = TRUE
  )
}
```


## Materiais

-   [Slide: R-Ladies São Paulo](https://r-ladies-sao-paulo.github.io/2023-12-dados-espaciais/slides/slide-rladies-sp.html)

-   [Slide: Trabalhando com dados espaciais no R](https://r-ladies-sao-paulo.github.io/2023-12-dados-espaciais/slides/)

-   [Código da prática - Parte 1](https://github.com/R-Ladies-Sao-Paulo/2023-12-dados-espaciais/blob/main/pratica-parte-1.R)

-   [Código da prática - Parte 2](https://github.com/R-Ladies-Sao-Paulo/2023-12-dados-espaciais/blob/main/pratica-parte-2.R)
