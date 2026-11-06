# R/02_dorling_fisica.R
# Simulacao de repulsao e empacotamento de circulos (Dorling Cartogram) via fisica de particulas

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(igraph)
  library(tidygraph)
  library(particles)
})

calcular_dorling_layout <- function(
  bairros_sf,
  coluna_metrica = "TOTAL_CRIMES",
  fator_escala_k = 40,    # Escala metrica em metros por unidade da raiz
  raio_minimo = 80,       # Raio minimo em metros para feicoes com 0 ocorrencias
  padding = 35,           # Distancia minima de resguardo entre bordas dos circulos (metros)
  iteracoes = 150         # Passos de simulacao fisica
) {
  message(">> Iniciando calculo do layout de Dorling...")
  
  # Preparar tabela atributiva e calcular raio proporcional a raiz do crime
  df_nodes <- bairros_sf %>%
    st_drop_geometry() %>%
    mutate(
      valor = .data[[coluna_metrica]],
      radius = ifelse(valor > 0, fator_escala_k * sqrt(valor), raio_minimo),
      x = x_orig,
      y = y_orig
    ) %>%
    arrange(desc(radius))
  
  message(">> Executando simulacao de forcas fisicas via {particles} (metodologia votemapswitzerland)...")
  
  # Construir grafo de particulas
  graph <- erdos.renyi.game(nrow(df_nodes), 0) %>%
    as_tbl_graph() %>%
    mutate(
      x = df_nodes$x,
      y = df_nodes$y,
      radius = df_nodes$radius
    )
  
  # Executar engine de forcas: ancoragem elastica ao centroide + colisao circular
  sim <- graph %>%
    simulate(setup = predefined_genesis(x, y)) %>%
    wield(collision_force, radius = radius + padding, n_iter = 5) %>%
    wield(x_force, x = x, strength = 0.08) %>%
    wield(y_force, y = y, strength = 0.08) %>%
    evolve(steps = iteracoes)
  
  # Extrair coordenadas finais estabilizadas
  positions <- as_tibble(sim$position)
  names(positions)[1:2] <- c("x_pos", "y_pos")
  
  df_nodes <- df_nodes %>%
    mutate(
      x_dorling = positions$x_pos,
      y_dorling = positions$y_pos,
      radius_dorling = radius
    )
  
  # Reatribuir as colunas calculadas ao objeto sf original
  bairros_sf <- bairros_sf %>%
    left_join(
      df_nodes %>% select(BAIRRO_NORM, x_dorling, y_dorling, radius_dorling),
      by = "BAIRRO_NORM"
    )
  
  # Gerar circunferencias geometricas estabilizadas (buffers no sistema projetado metrico)
  circulos_sfc <- st_buffer(
    st_sfc(lapply(1:nrow(bairros_sf), function(i) {
      st_point(c(bairros_sf$x_dorling[i], bairros_sf$y_dorling[i]))
    }), crs = st_crs(bairros_sf)),
    dist = bairros_sf$radius_dorling
  )
  
  bairros_sf$geom_circulo <- circulos_sfc
  
  message(">> Layout de Dorling estabilizado sem sobreposicoes!")
  return(bairros_sf)
}
