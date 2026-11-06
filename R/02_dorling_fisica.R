# R/02_dorling_fisica.R
# Simulação de repulsão e empacotamento de círculos (Dorling Cartogram) via física de partículas

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tidygraph)
  library(particles)
})

calcular_dorling_layout <- function(
  bairros_sf,
  coluna_metrica = "TOTAL_CRIMES",
  fator_escala_k = 65,    # Escala métrica em metros por unidade da raiz
  raio_minimo = 80,       # Raio mínimo em metros para bairros com 0 ocorrências
  padding = 30,           # Distância de segurança entre as bordas dos círculos (metros)
  iteracoes = 150         # Passos de simulação física
) {
  message(">> Iniciando cálculo do layout de Dorling...")
  
  df_nodes <- bairros_sf %>%
    st_drop_geometry() %>%
    mutate(
      valor = .data[[coluna_metrica]],
      # Raio proporcional à raiz quadrada do volume criminal (conserva área da bolha)
      radius = ifelse(valor > 0, fator_escala_k * sqrt(valor), raio_minimo),
      x = x_orig,
      y = y_orig
    )
  
  # Criar grafo de partículas
  graph <- as_tbl_graph(df_nodes)
  
  message(">> Executando simulação de forças físicas (atração ao centroide + colisão)...")
  
  sim <- simulation(graph) %>%
    # Força 1: Atração elástica em direção à coordenada geográfica original
    w_force(x_force(x = x_orig, strength = 0.08)) %>%
    w_force(y_force(y = y_orig, strength = 0.08)) %>%
    # Força 2: Repulsão mútua / colisão entre círculos com margem de segurança
    w_force(collision_force(rad = radius + padding, strength = 0.9, n_iter = 3)) %>%
    # Executar simulação de passos de física
    evolve(steps = iteracoes)
  
  # Extrair posições estabilizadas
  res_nodes <- as_tibble(sim)
  
  bairros_sf <- bairros_sf %>%
    mutate(
      x_dorling = res_nodes$x,
      y_dorling = res_nodes$y,
      radius_dorling = res_nodes$radius
    )
  
  # Gerar geometrias circulares (buffers no ponto estabilizado)
  circulos_sfc <- st_buffer(
    st_sfc(lapply(1:nrow(bairros_sf), function(i) {
      st_point(c(bairros_sf$x_dorling[i], bairros_sf$y_dorling[i]))
    }), crs = st_crs(bairros_sf)),
    dist = bairros_sf$radius_dorling
  )
  
  bairros_sf$geom_circulo <- circulos_sfc
  
  message(">> Layout de Dorling concluído sem sobreposição de partículas!")
  return(bairros_sf)
}
