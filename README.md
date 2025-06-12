# Análise Estatística de Dados de Alzheimer

## Objetivo

Este projeto realiza uma análise estatística sobre um conjunto de dados de pacientes com Alzheimer, focando em propriedades da média amostral, variância e simulações de amostragem. O objetivo é demonstrar, de forma prática, conceitos fundamentais de inferência estatística, como convergência da média amostral e comportamento da variância das médias.

---

## Estrutura do Projeto

```
trabalho-alzheimer/
│
├── data/
│   └── alzheimer_data.csv           # Arquivo de dados principal (CSV)
│
├── output/
│   ├── graficos/
│   │   ├── grafico1_medias_amostrais.png
│   │   └── grafico2_variancias_medias.png
│   └── resultados/
│       ├── interpretacao_graficos.txt
│       └── resultados_amostragem.csv
│
├── scripts/
│   └── exercicio_parte2.R           # Script de análise estatística
│
├── main.R                          # Script principal de execução
├── requirements.R                  # Instalação automática de dependências
└── README.md                       # Este documento
```

---

## Pré-requisitos

- **R** instalado (versão 4.0 ou superior)
- **Pacotes R:**
  - `ggplot2`
  - `dplyr`
  - `readr`

> **Obs:** Os pacotes são instalados automaticamente ao rodar o `requirements.R`.

- **Arquivo de dados:**
  - O arquivo `alzheimer_data.csv` deve estar na pasta `data/` e seguir o padrão CSV (separador vírgula, cabeçalho).

---

## Como Rodar o Projeto

### 1. Instale as dependências

No terminal ou console do R:
```r
source("requirements.R")
```

### 2. Execute a análise

No terminal, RStudio ou Cursor:
```r
source("main.R")
```

- O script irá criar as pastas necessárias, verificar o arquivo de dados e rodar toda a análise automaticamente.

### 3. Saídas geradas

Após a execução, serão criados:
- `output/graficos/grafico1_medias_amostrais.png`  
  Gráfico da convergência da média amostral para a média populacional.
- `output/graficos/grafico2_variancias_medias.png`  
  Gráfico da variância das médias amostrais em função do tamanho da amostra.
- `output/resultados/resultados_amostragem.csv`  
  Tabela com os resultados das simulações.
- `output/resultados/interpretacao_graficos.txt`  
  Interpretação automática dos gráficos e conclusões estatísticas.

---

## Funcionamento dos Scripts

- **main.R**: Script principal. Cria pastas, verifica dependências, checa o dataset e executa a análise.
- **requirements.R**: Instala automaticamente os pacotes necessários.
- **scripts/exercicio_parte2.R**: Faz toda a análise estatística, simulações, geração de gráficos e interpretação dos resultados.

---

## Sobre o Código de Análise

- O script lê o arquivo `data/alzheimer_data.csv`.
- Seleciona a variável quantitativa de interesse (por padrão, a idade).
- Realiza simulações de amostragem aleatória simples para diferentes tamanhos de amostra.
- Calcula médias e variâncias amostrais.
- Gera dois gráficos principais:
  - **Gráfico 1:** Mostra como a média amostral converge para a média populacional conforme o tamanho da amostra aumenta.
  - **Gráfico 2:** Mostra como a variância das médias amostrais diminui com o aumento do tamanho da amostra.
- Salva os resultados e uma interpretação automática dos gráficos.
- Os gráficos são salvos com fundo branco para facilitar a visualização e impressão.

---

## Dicas e Problemas Comuns

- **Erro de pacote não encontrado:** Rode `source("requirements.R")` novamente.
- **Arquivo de dados não encontrado:** Certifique-se de que `alzheimer_data.csv` está na pasta `data/`.
- **Rodando no Cursor ou RStudio:** Use sempre `source("main.R")` no console.
- **Rodando no terminal:** Se `Rscript` estiver disponível, pode usar:
  ```bash
  Rscript requirements.R
  Rscript main.R
  ```
- **Gráficos com fundo preto:** Já está corrigido, mas se acontecer, confira se o argumento `bg = "white"` está nas funções `ggsave`.

---

## Contato

Em caso de dúvidas ou problemas, abra uma issue ou entre em contato com o responsável pelo projeto. 
