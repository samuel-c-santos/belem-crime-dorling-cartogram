"""
01_extrair_siac.py
Extração automatizada de dados de criminalidade da SEGUP/PA (SIAC / CODEC)
Recorte: Município de Belém
"""

import sys
import os
import io
import json
import logging
from unicodedata import normalize
import requests
import pandas as pd
import urllib3

urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

logging.basicConfig(level=logging.INFO, format="%(asctime)s [%(levelname)s] %(message)s")

BASE_URL = "https://codec.segup.pa.gov.br"
API_BAIRROS_URL = f"{BASE_URL}/api/bairros"
DOWNLOAD_URL = f"{BASE_URL}/download_recorte"

# Dicionário de-para: grafia do SIAC -> grafia padrão do GeoJSON de Belém
DE_PARA_BAIRROS = {
    "TELEGRAFO SEM FIO": "TELEGRAFO",
    "VAL DE CANS": "VAL-DE-CAES",
    "VAL-DE-CANS": "VAL-DE-CAES",
    "TERRA FIRME": "MONTESE (TERRA FIRME)",
    "MONTESE": "MONTESE (TERRA FIRME)",
    "CURIO-UTINGA": "CURIO UTINGA",
    "CURIO": "CURIO UTINGA",
    "AGUAS LINDAS": "AGUAS LINDAS",
    "GUANABARA": "GUANABARA"
}

def clean_text(text: str) -> str:
    if not isinstance(text, str):
        return ""
    return normalize("NFKD", text).encode("ASCII", "ignore").decode("ASCII").upper().strip()

def extrair_dados_siac(data_inicio: str = "2024-01-01", data_fim: str = "2024-06-30", output_dir: str = "data"):
    sessao = requests.Session()
    sessao.verify = False

    headers = {
        "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
        "Referer": BASE_URL,
        "Origin": BASE_URL
    }

    logging.info(f"1. Obtendo lista oficial de bairros de Belém via API do SIAC...")
    resp_bairros = sessao.get(API_BAIRROS_URL, headers=headers, timeout=20)
    resp_bairros.raise_for_status()
    bairros_json = resp_bairros.json()

    bairros_belem = [
        b["bairros"] for b in bairros_json
        if clean_text(b.get("municipios", "")) == "BELEM"
    ]
    logging.info(f"Total de {len(bairros_belem)} bairros identificados para Belém na base SEGUP.")

    logging.info("2. Capturando csrftoken da página inicial...")
    resp_home = sessao.get(BASE_URL, headers=headers, timeout=20)
    csrftoken = sessao.cookies.get("csrftoken")
    if not csrftoken:
        raise RuntimeError("Não foi possível capturar o cookie csrftoken do Django.")
    logging.info(f"CSRF Token obtido: {csrftoken[:12]}...")

    logging.info(f"3. Montando payload de busca para o período {data_inicio} até {data_fim}...")
    payload = [
        ("csrfmiddlewaretoken", csrftoken),
        ("lista_municipios", "BELEM")
    ]
    for b in bairros_belem:
        payload.append(("lista_bairros", b))

    # Consolidados disponíveis no SIAC
    consolidados = [
        "ESTUPRO",
        "ESTUPRO DE VULNERAVEL",
        "FEMINICIDIO",
        "FURTO",
        "HOMICIDIO",
        "LATROCINIO",
        "LESAO CORPORAL",
        "ROUBO",
        "TRAFICO DE DROGAS"
    ]
    for c in consolidados:
        payload.append(("lista_consolidados", c))

    payload.append(("data_inicio", data_inicio))
    payload.append(("data_fim", data_fim))
    payload.append(("btn-pesquisa", "BUSCAR"))

    logging.info("4. Enviando requisição POST ao formulário do SIAC...")
    resp_post = sessao.post(BASE_URL, data=payload, headers=headers, timeout=60)
    resp_post.raise_for_status()

    if "PESQUISA REALIZADA COM SUCESSO" not in resp_post.text:
        logging.warning("O servidor respondeu, mas a mensagem de sucesso não foi detectada. Verifique parâmetros.")

    logging.info("5. Baixando planilha completa de /download_recorte...")
    resp_download = sessao.get(DOWNLOAD_URL, headers=headers, timeout=120)
    resp_download.raise_for_status()

    os.makedirs(os.path.join(output_dir, "raw"), exist_ok=True)
    os.makedirs(os.path.join(output_dir, "processed"), exist_ok=True)

    raw_xlsx_path = os.path.join(output_dir, "raw", f"siac_crimes_belem_{data_inicio}_a_{data_fim}.xlsx")
    with open(raw_xlsx_path, "wb") as f:
        f.write(resp_download.content)
    logging.info(f"Arquivo bruto salvo em: {raw_xlsx_path} ({len(resp_download.content)} bytes)")

    logging.info("6. Processando e normalizando dados com Pandas...")
    df = pd.read_excel(io.BytesIO(resp_download.content))
    logging.info(f"Total de ocorrências extraídas: {len(df)}")

    # Padronizar nomes de colunas
    df.columns = [clean_text(c).replace(" ", "_").replace("(", "").replace(")", "") for c in df.columns]

    # Limpar coordenadas
    if "LATITUDE" in df.columns and "LONGITUDE" in df.columns:
        df["LATITUDE"] = df["LATITUDE"].astype(str).str.replace(",", ".").str.strip()
        df["LONGITUDE"] = df["LONGITUDE"].astype(str).str.replace(",", ".").str.strip()
        df["LATITUDE"] = pd.to_numeric(df["LATITUDE"], errors="coerce")
        df["LONGITUDE"] = pd.to_numeric(df["LONGITUDE"], errors="coerce")

    # Normalizar Bairro
    bairro_col = "BAIRROS" if "BAIRROS" in df.columns else "BAIRRO"
    df["BAIRRO_ORIGINAL"] = df[bairro_col].astype(str)
    df["BAIRRO_CLEAN"] = df["BAIRRO_ORIGINAL"].apply(clean_text)
    df["BAIRRO_NORMALIZADO"] = df["BAIRRO_CLEAN"].map(lambda x: DE_PARA_BAIRROS.get(x, x))

    # Salvar dataset detalhado
    processed_csv = os.path.join(output_dir, "processed", "crimes_belem_detalhado.csv")
    df.to_csv(processed_csv, index=False, encoding="utf-8")
    logging.info(f"Dataset detalhado salvo em: {processed_csv}")

    # Agregação por Bairro
    crime_col = "CONSOLIDADOS" if "CONSOLIDADOS" in df.columns else "CONSOLIDADO"
    
    agregado = df.groupby("BAIRRO_NORMALIZADO").agg(
        TOTAL_CRIMES=(crime_col, "count"),
        ROUBO=(crime_col, lambda s: (s == "ROUBO").sum()),
        FURTO=(crime_col, lambda s: (s == "FURTO").sum()),
        HOMICIDIO=(crime_col, lambda s: (s == "HOMICIDIO").sum()),
        LATROCINIO=(crime_col, lambda s: (s == "LATROCINIO").sum()),
        TRAFICO_DROGAS=(crime_col, lambda s: (s == "TRAFICO DE DROGAS").sum())
    ).reset_index()

    # Crimes Violentos Letais Intencionais (CVLI) = Homicídio + Latrocínio + Lesão Corporal Seguida de Morte
    agregado["CVLI"] = agregado["HOMICIDIO"] + agregado["LATROCINIO"]
    agregado = agregado.sort_values(by="TOTAL_CRIMES", ascending=False)

    agregado_csv = os.path.join(output_dir, "processed", "crimes_bairros_agregado.csv")
    agregado.to_csv(agregado_csv, index=False, encoding="utf-8")
    logging.info(f"Dataset agregado salvo em: {agregado_csv}")
    logging.info("Top 5 bairros mais incidentes:")
    print(agregado.head(5).to_string(index=False))

    return df, agregado

if __name__ == "__main__":
    ini = sys.argv[1] if len(sys.argv) > 1 else "2024-01-01"
    fim = sys.argv[2] if len(sys.argv) > 2 else "2024-03-31"
    out = sys.argv[3] if len(sys.argv) > 3 else "data"
    extrair_dados_siac(ini, fim, out)
