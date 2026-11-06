# R/01_geometrias_bairros.R
# Tratamento geoespacial da malha de Bairros de Belém e junção com dados do SIAC/SEGUP

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(stringi)
})

# Desativar motor esferico S2 para evitar conflito com vertices planares (utiliza engine GEOS padrão)
sf_use_s2(FALSE)

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
  message(">> Carregando malha de bairros de Belem...")
  bairros_raw <- st_read(geojson_path, quiet = TRUE)
  
  # Normalizar nomes
  bairros_raw <- bairros_raw %>%
    mutate(
      BAIRRO_NORM = normalizar_texto(nome)
    )
  
  # Validar geometrias planas
  message(">> Validando e corrigindo topologia com GEOS...")
  bairros_valid <- st_make_valid(bairros_raw)
  
  # Reprojetar para CRS Metrico (SIRGAS 2000 UTM 22S)
  message(">> Reprojetando para EPSG:", crs_metrico, " (SIRGAS 2000 / UTM 22S)...")
  bairros_utm <- st_transform(bairros_valid, crs = crs_metrico)
  
  # Assegurar geometria simples POLYGON
  bairros_utm <- bairros_utm %>%
    st_cast("POLYGON") %>%
    mutate(area_feicao = as.numeric(st_area(geometry))) %>%
    group_by(BAIRRO_NORM) %>%
    slice_max(area_feicao, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(-area_feicao)
  
  # Calcular centroides na superficie
  message(">> Calculando centroides na superficie (st_point_on_surface)...")
  centroides <- st_point_on_surface(bairros_utm)
  coords_centroides <- st_coordinates(centroides)
  
  bairros_utm <- bairros_utm %>%
    mutate(
      x_orig = coords_centroides[, 1],
      y_orig = coords_centroides[, 2],
      area_km2 = as.numeric(st_area(geometry)) / 1e6
    )
  
  # Carregar agregacao de crimes do SIAC
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
  
  message(">> Concluido! Bairros processados com sucesso: ", nrow(bairros_dados))
  return(bairros_dados)
}
