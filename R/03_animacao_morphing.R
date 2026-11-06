# R/03_animacao_morphing.R
# Interpolacao geometrica (Polygon Morphing) e renderizacao animada no padrao votemapswitzerland

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(gganimate)
  library(transformr)
  library(viridis)
  library(gifski)
})

gerar_animacao_cartograma <- function(
  bairros_dorling_sf,
  coluna_valor = "TOTAL_CRIMES",
  output_file = "outputs/animacao_dorling_seguranca_belem.gif",
  n_frames = 80,
  fps = 16,
  largura = 1000,
  altura = 800
) {
  message(">> Preparando estados geometricos para interpolacao (morphing)...")
  
  # Estado 1: Malha original de poligonos unificados
  estado_mapa <- bairros_dorling_sf %>%
    mutate(
      estado_id = 1,
      estado_nome = "1. Limite Territorial Real dos Bairros"
    ) %>%
    select(BAIRRO_NORM, valor = all_of(coluna_valor), estado_id, estado_nome, geometry)
  
  # Estado 2: Cartograma de Circulos (Dorling)
  estado_dorling <- bairros_dorling_sf %>%
    mutate(
      estado_id = 2,
      estado_nome = "2. Cartograma de Dorling (Raio Proporcional as Ocorrencias)"
    ) %>%
    st_set_geometry("geom_circulo") %>%
    select(BAIRRO_NORM, valor = all_of(coluna_valor), estado_id, estado_nome, geometry = geom_circulo)
  
  # Assegurar mesmo CRS em ambos os estados
  st_crs(estado_dorling) <- st_crs(estado_mapa)
  
  # Unir os dois estados para animacao
  dados_combinados <- rbind(estado_mapa, estado_dorling)
  
  message(">> Montando composicao grafica com ggplot2...")
  
  p <- ggplot(dados_combinados) +
    geom_sf(aes(fill = valor, group = BAIRRO_NORM), color = "#21262d", size = 0.3) +
    scale_fill_viridis_c(
      option = "inferno",
      direction = -1,
      name = "Ocorrencias\nRegistradas\n(SIAC/SEGUP)",
      labels = scales::comma
    ) +
    theme_void(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "#0d1117", color = NA),
      panel.background = element_rect(fill = "#0d1117", color = NA),
      legend.background = element_rect(fill = "#0d1117", color = NA),
      text = element_text(color = "#e6edf3"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#58a6ff", margin = margin(t = 12, b = 4)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#8b949e", margin = margin(b = 12)),
      plot.caption = element_text(size = 9, hjust = 0.5, color = "#6e7681", margin = margin(t = 12, b = 8)),
      legend.position = "right",
      legend.text = element_text(color = "#c9d1d9"),
      legend.title = element_text(color = "#f0f6fc", face = "bold")
    ) +
    labs(
      title = "DISTRIBUICAO ESPACIAL DE OCORRENCIAS CRIMINAIS EM BELEM (PA)",
      subtitle = "{closest_state}",
      caption = "Fonte dos dados: SIAC/CODEC - SEGUP/PA | Malha vetorial: GeoVisor Belem | Metodologia: Cartograma de Dorling"
    ) +
    transition_states(
      states = estado_nome,
      transition_length = 3,
      state_length = 2
    ) +
    ease_aes("cubic-in-out")
  
  message(">> Renderizando animacao via gganimate + gifski (", n_frames, " quadros)...")
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  
  anim <- animate(
    p,
    nframes = n_frames,
    fps = fps,
    width = largura,
    height = altura,
    res = 96,
    renderer = gifski_renderer(file = output_file)
  )
  
  message(">> Animacao gravada com sucesso em: ", output_file)
  return(output_file)
}
