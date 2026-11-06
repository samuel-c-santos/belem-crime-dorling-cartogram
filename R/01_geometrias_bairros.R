# R/01_geometrias_bairros.R
# Tratamento geoespacial da malha de Bairros de Belém e junção com dados do SIAC/SEGUP

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(stringi)
})

normalizar_texto <- function(texto) {
  texto %>%
    stri_trans_general("Latin-ASCII") %>%
    toupper() %>%
    trimws()
}

preparar_geometrias_bairros <- function(
  geojson_path = "data/raw/bairros_belem.geojson",
  crimes_csv_path = "data/processed/crimes_bairros_agregado.csv",
  crs_metrico = 31982 # SIRGAS 2000 / UTM zone 22S
) {
  message(">> Carregando malha de bairros de Belém...")
  bairros <- st_read(geojson_path, quiet = TRUE)
  
  # Normalizar nomes
  bairros <- bairros %>%
    mutate(
      BAIRRO_NORM = normalizar_texto(nome)
    )
  
  # Reprojetar para CRS Métrico (metros) para cálculos físicos corretos
  message(">> Reprojetando para EPSG:", crs_metrico, " (SIRGAS 2000 / UTM 22S)...")
  bairros_utm <- st_transform(bairros, crs = crs_metrico)
  
  # Calcular centroides na superfície
  message(">> Calculando centroides na superfície (st_point_on_surface)...")
  centroides <- st_point_on_surface(bairros_utm)
  coords_centroides <- st_coordinates(centroides)
  
  bairros_utm <- bairros_utm %>%
    mutate(
      x_orig = coords_centroides[, 1],
      y_orig = coords_centroides[, 2],
      area_km2 = as.numeric(st_area(geometry)) / 1e6
    )
  
  # Carregar agregação de crimes do SIAC
  message(">> Cruzando com dados de crimes do SIAC/SEGUP...")
  crimes <- read_csv(crimes_csv_path, show_col_types = FALSE)
  
  # Join espacial/atributivo
  bairros_dados <- bairros_utm %>%
    left_join(crimes, by = c("BAIRRO_NORM" = "BAIRRO_NORMALIZADO")) %>%
    mutate(
      TOTAL_CRIMES = coalesce(TOTAL_CRIMES, 0),
      ROUBO = coalesce(ROUBO, 0),
      FURTO = coalesce(FURTO, 0),
      HOMICIDIO = coalesce(HOMICIDIO, 0),
      CVLI = coalesce(CVLI, 0),
      TRAFICO_DROGAS = coalesce(TRAFICO_DROGAS, 0),
      TAXA_CRIMES_KM2 = TOTAL_CRIMES / area_km2
    )
  
  message(">> Concluído! Bairros processados: ", nrow(bairros_dados))
  return(bairros_dados)
}
