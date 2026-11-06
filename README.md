# Cartograma de Dorling para Analise de Criminalidade Urbana em Belem (PA)

[![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/samuel-c-santos/belem-crime-dorling-cartogram/blob/main/notebooks/colab_runner.ipynb)

Pipeline computacional integrado em Python e R para extracao de registros criminais do Sistema Integrado de Seguranca Publica (SIAC/SEGUP-PA), tratamento geoespacial metrico e renderizacao de cartograma animado de Dorling com interpolacao geometrica continua (polygon-to-circle morphing).

## 1. Descricao Tecnica

A representacao coropletica convencional baseada em limites politico-administrativos introduz distorcao perceptiva em analises de fenomenos socioespaciais. Bairros com grande extensao territorial e baixa densidade demografica, como as ilhas e zonas rurais de Belem (Mosqueiro, Outeiro e Cotijuba), ocupam visualmente a maior parte da superficie cartografica, enquanto areas densamente urbanizadas de alta concentracao de ocorrencias (como Marco, Pedreira e Umarizal) sao sub-representadas.

Este projeto implementa uma solucao metodologica inspirada no modelo algoritmico do repositorio `votemapswitzerland`. Atraves de simulacao fisica de particulas e tecnicas de deformacao geometrica, a malha vetorial dos bairros e progressivamente convertida em esferas cujas areas superficiais sao proporcionais a frequencia absoluta de ocorrencias registradas. O algoritmo de dispersao evita a sobreposicao espacial preservando, na medida do possivel, a topologia e a vizinhanca geografica original.

## 2. Estrutura do Diretorio

```text
cartografia-seguranca-belem/
├── data/
│   ├── raw/
│   │   ├── bairros_belem.geojson         # Malha poligonal original em coordenadas geograficas (WGS84)
│   │   └── siac_crimes_belem_*.xlsx      # Planilhas brutas extraidas do portal da SEGUP/PA
│   └── processed/
│       ├── crimes_belem_detalhado.csv    # Registros limpos com coordenadas tratadas e validadas
│       └── crimes_bairros_agregado.csv   # Consolidado estatistico por bairro (Roubos, Furtos, CVLI)
├── scripts/
│   └── 01_extrair_siac.py                # Script de ingestao automatizada via protocolo HTTP POST
├── R/
│   ├── 01_geometrias_bairros.R           # Reprojecao metrica (SIRGAS 2000 / UTM 22S) e calculo de centroides
│   ├── 02_dorling_fisica.R               # Simulacao de forcas de colisao e repulsao de particulas
│   └── 03_animacao_morphing.R            # Interpolacao continua de vertices e renderizacao grafica
├── notebooks/
│   └── colab_runner.ipynb                # Caderno orquestrador para execucao em ambiente Google Colab
├── outputs/                              # Arquivos finais renderizados (.gif, .mp4)
├── diretriz.md                           # Especificacao documental de arquitetura
└── README.md
```

## 3. Especificacao dos Modulos

### 3.1. Ingestao e Normalizacao (Python)
O script `scripts/01_extrair_siac.py` automatiza o fluxo de coleta de dados no portal publico da Coordenadoria de Estatistica e Analise Criminal (CODEC/SEGUP):
1. Requisicao inicial GET para instanciacao de sessao persistente e captura de cookie `csrftoken`.
2. Consulta a API interna `/api/bairros` para obtencao dos identificadores nominais dos bairros vinculados a 01ª Regiao de Integracao de Seguranca Publica (RISP).
3. Envio de requisicao POST estruturada contendo parametros de periodo temporal, lista de bairros e classes de crimes consolidados.
4. Download do recorte em formato de planilha eletronica atraves do endpoint `/download_recorte`.
5. Tratamento de coordenadas geograficas (`LATITUDE` e `LONGITUDE`), conversao para ponto flutuante de precisao dupla e aplicacao de tabela de equivalencia nominal (de-para) para compatibilizacao com a malha vetorial do municipio.

### 3.2. Projecao Metrica e Agregacao (R)
O modulo `R/01_geometrias_bairros.R` opera sobre a biblioteca `sf`:
1. Conversao do sistema de referencia de coordenadas geograficas para o sistema cartografico plano projetado SIRGAS 2000 / UTM zone 22S (`EPSG:31982`). Essa etapa e obrigatoria para assegurar que operacoes de distancia, raios e forcas fisicas sejam computadas em metros lineares.
2. Identificacao de pontos internos de ancoragem utilizando a funcao `st_point_on_surface`, garantindo que o ponto de gravidade esteja contido na superficie do poligono correspondente, mesmo em geometrias concavas ou multipartes.
3. Juncao relacional entre a malha vetorial e o arquivo tabular consolidado de delitos.

### 3.3. Simulacao Fisica de Particulas (R)
O modulo `R/02_dorling_fisica.R` utiliza os pacotes `particles` e `tidygraph`:
1. Definicao do raio nominal $R_i$ de cada feicao a partir da raiz quadrada da metrica de ocorrencias:
   $$R_i = k \cdot \sqrt{\text{Crimes}_i}$$
2. Configuracao de forca de atracao direcional em relacao as coordenadas metricas originais do centroide do bairro.
3. Configuracao de forca de exclusao mutua baseada em colisao circular com distancia minima de resguardo (`padding`).
4. Iteracao do sistema dinâmico ate convergencia estatica das posicoes.

### 3.4. Interpolacao Geometrica e Renderizacao (R)
O modulo `R/03_animacao_morphing.R` emprega `transformr`, `gganimate` e `gifski`:
1. Mapeamento dos vertices correspondentes entre os limites poligonais originais e as coordenadas das circunferencias geradas.
2. Interpolacao linear e cubica dos estados geometricos em taxa fixa de quadros por segundo.
3. Exportacao do artefato grafico em resolucao definida para analise documental.

## 4. Requisitos e Procedimentos de Execucao

### 4.1. Ambiente Local (Extracao)
Requisitos: Python 3.10 ou superior, `requests`, `pandas`, `openpyxl`.

Comando de execucao (modo automatico: janela deslizante dos ultimos 365 dias):
```bash
python scripts/01_extrair_siac.py
```

Ou com delimitacao manual de datas:
```bash
python scripts/01_extrair_siac.py "2023-01-01" "2023-12-31" "data"
```

### 4.2. Ambiente de Nuvem (Google Colab)
O processamento computacional intensivo de particulas e a renderizacao quadro a quadro sao estruturados para execucao no Google Colab sob ambiente Linux (Ubuntu) com kernel R nativo.

1. Abrir o caderno atraves do atalho de execucao: [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/samuel-c-santos/belem-crime-dorling-cartogram/blob/main/notebooks/colab_runner.ipynb)
2. Executar a celula de configuracao para instalacao dos pacotes de sistema (`libgdal-dev`, `libgeos-dev`, `libproj-dev`, `libudunits2-dev`) e bibliotecas R correspondentes.
3. Executar o fluxo sequencial de calculo e renderizacao.

## 5. Referencias e Citacoes

1. **Dorling, D. (1996).** *Area Cartograms: Their Use and Creation.* Concepts and Techniques in Modern Geography (CATMOG), No. 59. University of East Anglia, Norwich: Environmental Publications.
2. **Zumbach, D. (2020).** *votemapswitzerland: A Swiss version of the famous visualization «Land doesn't vote, people do».* SRF Data / GitHub repository. Disponivel em: <https://github.com/zumbov2/votemapswitzerland>.
3. **Secretaria de Estado de Seguranca Publica e Defesa Social do Para (SEGUP/PA).** *Sistema Integrado de Seguranca Publica (SIAC).* Coordenadoria de Estatistica e Analise Criminal (CODEC). Dados abertos de ocorrencias criminais. Disponivel em: <https://codec.segup.pa.gov.br/>.
4. **Pedersen, T. L. (2020).** *transformr: Polygon and Path Morphing; particles: A Simulation of Particle Systems; gganimate: A Grammar of Animated Graphics.* R packages.
5. **Prefeitura Municipal de Belem / CODEM.** *Malha de Bairros Oficiais do Municipio de Belem.* Base vetorial consolidada via projeto GeoVisor Belem.
