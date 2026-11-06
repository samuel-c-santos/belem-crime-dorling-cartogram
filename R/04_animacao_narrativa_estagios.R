# R/04_animacao_narrativa_estagios.R
# Animacao em quatro atos com agrupamento por Severidade Estatistica (David Zumbach)

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(gganimate)
  library(transformr)
  library(viridis)
  library(gifski)
  library(igraph)
  library(tidygraph)
  library(particles)
})

gerar_animacao_votemap2 <- function(
  bairros_sf,
  coluna_valor = "TOTAL_CRIMES",
  output_file = "outputs/animacao_narrativa_estagios_belem.gif",
  cor_fundo = "#ffffff",
  n_frames = 110,
  fps = 16,
  largura = 800,
  altura = 800
) {
  message(">> Iniciando construcao dos estagios...")
  
  # 1. Definir media municipal e classificacao de severidade afirmativa
  media_crimes <- mean(bairros_sf[[coluna_valor]], na.rm = TRUE)
  bairros_sf <- bairros_sf %>%
    mutate(
      valor = .data[[coluna_valor]],
      severidade = factor(
        ifelse(valor >= media_crimes, "Acima da Media", "Abaixo da Media"),
        levels = c("Abaixo da Media", "Acima da Media")
      )
    )
  
  message(sprintf(">> Media municipal: %.1f ocorrencias.", media_crimes))
  
  # Coordenadas e extensoes da malha
  bbox_orig <- st_bbox(bairros_sf)
  largura_mapa <- bbox_orig$xmax - bbox_orig$xmin
  altura_mapa  <- bbox_orig$ymax - bbox_orig$ymin
  centro_x     <- (bbox_orig$xmax + bbox_orig$xmin) / 2
  centro_y     <- (bbox_orig$ymax + bbox_orig$ymin) / 2
  
  # =========================================================================
  # ATO 1: Malha Territorial dos Bairros
  # =========================================================================
  estado_1 <- bairros_sf %>%
    mutate(
      fase_id = 1,
      fase_label = "1. Malha Territorial dos Bairros"
    ) %>%
    select(BAIRRO_NORM, valor, severidade, fase_id, fase_label, geometry)
  
  # =========================================================================
  # ATO 2: Ordenamento em Grade por Extensao Territorial (9 colunas x 8 linhas)
  # =========================================================================
  message(">> Calculando distribuicao da grade...")
  
  n_cols <- 9
  n_rows <- 8
  
  step_x <- largura_mapa / n_cols
  step_y <- altura_mapa / n_rows
  
  bairros_grid <- bairros_sf %>%
    arrange(desc(area_km2)) %>%
    mutate(
      rank_area = row_number(),
      col_idx = (rank_area - 1) %% n_cols,
      row_idx = (rank_area - 1) %/% n_cols,
      x_grid = bbox_orig$xmin + (col_idx + 0.5) * step_x,
      y_grid = bbox_orig$ymax - (row_idx + 0.5) * step_y
    )
  
  geoms_grid <- lapply(1:nrow(bairros_grid), function(i) {
    b_geom <- bairros_grid$geometry[[i]]
    cx <- bairros_grid$x_orig[i]
    cy <- bairros_grid$y_orig[i]
    gx <- bairros_grid$x_grid[i]
    gy <- bairros_grid$y_grid[i]
    
    bb <- st_bbox(b_geom)
    b_w <- max(bb$xmax - bb$xmin, 100)
    b_h <- max(bb$ymax - bb$ymin, 100)
    
    escala <- min((step_x * 0.75) / b_w, (step_y * 0.75) / b_h)
    (b_geom - c(cx, cy)) * escala + c(gx, gy)
  })
  
  estado_2 <- bairros_grid %>%
    mutate(
      fase_id = 2,
      fase_label = "2. Ordenamento em Grade por Extensao Territorial"
    ) %>%
    st_set_geometry(st_sfc(geoms_grid, crs = st_crs(bairros_sf))) %>%
    select(BAIRRO_NORM, valor, severidade, fase_id, fase_label, geometry)
  
  # =========================================================================
  # ATO 3: Dimensionamento Radial por Volume de Ocorrencias
  # =========================================================================
  message(">> Calculando dimensionamento radial...")
  
  raio_max_grid <- min(step_x, step_y) * 0.42
  max_valor <- max(bairros_grid$valor, na.rm = TRUE)
  
  circulos_grid <- lapply(1:nrow(bairros_grid), function(i) {
    gx <- bairros_grid$x_grid[i]
    gy <- bairros_grid$y_grid[i]
    val <- bairros_grid$valor[i]
    r <- max(sqrt(val / max_valor) * raio_max_grid, min(step_x, step_y) * 0.08)
    st_buffer(st_point(c(gx, gy)), dist = r)
  })
  
  estado_3 <- bairros_grid %>%
    mutate(
      fase_id = 3,
      fase_label = "3. Dimensionamento Radial por Volume de Ocorrencias"
    ) %>%
    st_set_geometry(st_sfc(circulos_grid, crs = st_crs(bairros_sf))) %>%
    select(BAIRRO_NORM, valor, severidade, fase_id, fase_label, geometry)
  
  # =========================================================================
  # ATO 4: Agrupamento em Enxames por Severidade Estatistica
  # =========================================================================
  message(">> Executando agrupamento por severidade...")
  
  x_centro_alta  <- bbox_orig$xmin + 0.32 * largura_mapa
  x_centro_baixa <- bbox_orig$xmin + 0.68 * largura_mapa
  y_centro_ambos <- centro_y
  
  empacotar_cluster <- function(df_sub, x_alvo, y_alvo, spacing = 50) {
    raio_max_cluster <- largura_mapa * 0.08
    df_nodes <- df_sub %>%
      arrange(desc(valor)) %>%
      mutate(
        radius = max(sqrt(valor / max_valor) * raio_max_cluster, largura_mapa * 0.015),
        x = x_alvo,
        y = y_alvo
      )
    
    g <- erdos.renyi.game(nrow(df_nodes), 0) %>%
      as_tbl_graph() %>%
      mutate(
        x = df_nodes$x,
        y = df_nodes$y,
        radius = df_nodes$radius
      )
    
    sim <- g %>%
      simulate(setup = predefined_genesis(x, y)) %>%
      wield(collision_force, radius = radius + spacing, n_iter = 6) %>%
      wield(x_force, x = x_alvo, strength = 0.12) %>%
      wield(y_force, y = y_alvo, strength = 0.12) %>%
      evolve(steps = 180)
    
    pos_mat <- as.matrix(sim$position)
    df_nodes$x_cluster <- pos_mat[, 1]
    df_nodes$y_cluster <- pos_mat[, 2]
    
    circs <- lapply(1:nrow(df_nodes), function(i) {
      st_buffer(st_point(c(df_nodes$x_cluster[i], df_nodes$y_cluster[i])), dist = df_nodes$radius[i])
    })
    
    df_nodes %>%
      st_set_geometry(st_sfc(circs, crs = st_crs(bairros_sf)))
  }
  
  cluster_alta  <- empacotar_cluster(bairros_grid %>% filter(severidade == "Acima da Media"), x_centro_alta, y_centro_ambos)
  cluster_baixa <- empacotar_cluster(bairros_grid %>% filter(severidade == "Abaixo da Media"), x_centro_baixa, y_centro_ambos)
  
  estado_4 <- bind_rows(cluster_alta, cluster_baixa) %>%
    mutate(
      fase_id = 4,
      fase_label = "4. Agrupamento em Enxames por Severidade Estatistica"
    ) %>%
    select(BAIRRO_NORM, valor, severidade, fase_id, fase_label, geometry)
  
  # =========================================================================
  # Consolidacao
  # =========================================================================
  todos_estados <- rbind(estado_1, estado_2, estado_3, estado_4)
  
  message(">> Montando composicao grafica...")
  
  familia_fonte <- {
    fams <- tryCatch(sysfonts::font_families(), error = function(e) character(0))
    if ("barlow" %in% fams) "barlow"
    else if ("inter" %in% fams) "inter"
    else if ("plex" %in% fams) "plex"
    else "sans"
  }
  
  p <- ggplot(todos_estados) +
    geom_sf(aes(fill = severidade, group = BAIRRO_NORM), color = "#475569", size = 0.25) +
    scale_fill_manual(
      values = c(
        "Abaixo da Media" = "#0284c7",
        "Acima da Media"  = "#b91c1c"
      ),
      name = "Severidade em Relacao a Media Municipal"
    ) +
    theme_void(base_size = 13) +
    theme(
      plot.background = element_rect(fill = cor_fundo, color = NA),
      panel.background = element_rect(fill = cor_fundo, color = NA),
      legend.background = element_rect(fill = cor_fundo, color = NA),
      plot.margin = margin(t = 16, r = 24, b = 16, l = 24),
      text = element_text(color = "#0f172a", family = familia_fonte),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5, color = "#0f172a", margin = margin(b = 4)),
      plot.subtitle = element_text(size = 11, face = "bold", hjust = 0.5, color = "#334155", margin = margin(b = 8)),
      plot.caption = element_text(size = 8.5, hjust = 0.5, color = "#64748b", margin = margin(t = 10, b = 4)),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      legend.box.just = "center",
      legend.justification = "center",
      legend.spacing.x = unit(0.4, "cm"),
      legend.title = element_text(size = 9.5, face = "bold", color = "#1e293b", hjust = 0.5, margin = margin(b = 4)),
      legend.text = element_text(size = 9, color = "#1e293b")
    ) +
    guides(
      fill = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        label.position = "right",
        keywidth = unit(1.2, "lines"),
        keyheight = unit(0.8, "lines")
      )
    ) +
    labs(
      title = "SEGURANCA PUBLICA EM BELEM (PA): DA GEOGRAFIA AO IMPACTO REAL",
      subtitle = "{closest_state}",
      caption = "Fonte: SIAC/CODEC - SEGUP/PA | Malha: GeoVisor Belem | Arquitetura: votemapswitzerland2.R (David Zumbach)"
    ) +
    transition_states(
      states = fase_label,
      transition_length = 3,
      state_length = 3
    ) +
    ease_aes("cubic-in-out")
  
  message(">> Renderizando animacao (", n_frames, " quadros)...")
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
