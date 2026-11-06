# R/03_animacao_morphing.R
# Interpolacao geometrica continua (Karim Douieb / Dorling In-Situ)

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
  cor_fundo = "#ffffff",
  n_frames = 80,
  fps = 16,
  largura = 800,
  altura = 800
) {
  message(">> Preparando estados geometricos para interpolacao in-situ...")
  
  # Estado 1: Malha original de limites dos bairros
  estado_mapa <- bairros_dorling_sf %>%
    mutate(
      estado_id = 1,
      estado_nome = "1. Limite Territorial Real dos Bairros"
    ) %>%
    select(BAIRRO_NORM, valor = all_of(coluna_valor), estado_id, estado_nome, geometry)
  
  # Estado 2: Cartograma de Dorling (circulos in-situ sem sobreposicao)
  estado_dorling <- bairros_dorling_sf %>%
    mutate(
      estado_id = 2,
      estado_nome = "2. Cartograma de Dorling (Raio Proporcional ao Volume de Ocorrencias)"
    ) %>%
    st_set_geometry("geom_circulo") %>%
    select(BAIRRO_NORM, valor = all_of(coluna_valor), estado_id, estado_nome, geometry = geom_circulo)
  
  st_crs(estado_dorling) <- st_crs(estado_mapa)
  dados_combinados <- rbind(estado_mapa, estado_dorling)
  
  message(">> Montando composicao grafica...")
  
  familia_fonte <- if ("plex" %in% tryCatch(sysfonts::font_families(), error = function(e) "")) "plex" else "sans"
  
  p <- ggplot(dados_combinados) +
    geom_sf(aes(fill = valor, group = BAIRRO_NORM), color = "#475569", size = 0.25) +
    scale_fill_viridis_c(
      option = "inferno",
      direction = -1,
      labels = scales::comma
    ) +
    theme_void(base_size = 13) +
    theme(
      plot.background = element_rect(fill = cor_fundo, color = NA),
      panel.background = element_rect(fill = cor_fundo, color = NA),
      legend.background = element_rect(fill = cor_fundo, color = NA),
      plot.margin = margin(t = 16, r = 16, b = 16, l = 16),
      text = element_text(color = "#0f172a", family = familia_fonte),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5, color = "#0f172a", margin = margin(b = 4)),
      plot.subtitle = element_text(size = 11, face = "bold", hjust = 0.5, color = "#475569", margin = margin(b = 8)),
      plot.caption = element_text(size = 8.5, hjust = 0.5, color = "#64748b", margin = margin(t = 10, b = 4)),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.title = element_text(size = 9.5, face = "bold", color = "#1e293b", margin = margin(b = 4)),
      legend.text = element_text(size = 8.5, color = "#334155")
    ) +
    guides(
      fill = guide_colorbar(
        title = "Ocorrencias Registradas no Periodo (SIAC/SEGUP)",
        title.position = "top",
        title.hjust = 0.5,
        barwidth = unit(18, "lines"),
        barheight = unit(0.7, "lines")
      )
    ) +
    labs(
      title = "DISTRIBUICAO ESPACIAL DE OCORRENCIAS CRIMINAIS EM BELEM (PA)",
      subtitle = "{closest_state}",
      caption = "Fonte dos dados: SIAC/CODEC - SEGUP/PA | Malha: GeoVisor Belem | Metodologia: Dorling In-Situ"
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
  
  message(">> Animacao salva em: ", output_file)
  return(output_file)
}
