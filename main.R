# CONFIGURAÇÃO INICIAL E LIMPEZA DO AMBIENTE

rm(list = ls())
options(warn = -1) # Suprimir warnings desnecessários durante execução

cat("=== ANÁLISE ESTATÍSTICA - ALZHEIMER DATASET ===\n")
cat("Implementação otimizada com validações robustas\n\n")

# VALIDAÇÃO E CRIAÇÃO DE ESTRUTURA DE DIRETÓRIOS

#' Cria estrutura de diretórios necessária
#' @description Função pura para setup inicial do projeto
create_directory_structure <- function() {
  required_dirs <- c(
    "data",
    "output",
    "output/graficos",
    "output/resultados"
  )

  for (dir_path in required_dirs) {
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
      cat(sprintf("✓ Diretório criado: %s\n", dir_path))
    }
  }
}

cat("1. Configurando estrutura de projeto...\n")
create_directory_structure()

# INSTALAÇÃO E CARREGAMENTO DE DEPENDÊNCIAS

#' Instala e carrega pacotes necessários de forma eficiente
#' @description Evita reinstalações desnecessárias
setup_dependencies <- function() {
  required_packages <- c("ggplot2", "dplyr", "readr")

  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE, quiet = TRUE)
      library(pkg, character.only = TRUE)
    }
  }

  cat("✓ Dependências configuradas\n")
}

cat("2. Configurando dependências...\n")
setup_dependencies()

# VALIDAÇÃO DO DATASET

#' Valida existência e integridade do dataset
#' @param file_path Caminho para o arquivo de dados
#' @return logical TRUE se válido
validate_dataset <- function(file_path = "data/alzheimer_data.csv") {
  if (!file.exists(file_path)) {
    cat("❌ ERRO CRÍTICO: Dataset não encontrado!\n")
    cat("📋 AÇÃO NECESSÁRIA:\n")
    cat("   1. Coloque 'alzheimer_data.csv' na pasta 'data/'\n")
    cat("   2. Execute novamente: source('main.R')\n\n")
    return(FALSE)
  }

  # Validação básica de conteúdo
  tryCatch(
    {
      test_data <- read.csv(file_path, nrows = 5)
      if (nrow(test_data) == 0 || ncol(test_data) < 2) {
        cat("❌ ERRO: Dataset vazio ou formato inválido\n")
        return(FALSE)
      }
      cat("✓ Dataset validado com sucesso\n")
      TRUE
    },
    error = function(e) {
      cat("❌ ERRO na leitura do dataset:", conditionMessage(e), "\n")
      FALSE
    }
  )
}

cat("3. Validando dataset...\n")
if (!validate_dataset()) {
  stop("Execução interrompida devido a problemas no dataset")
}

# EXECUÇÃO DA ANÁLISE PRINCIPAL

cat("4. Executando análise estatística...\n")

# Buscar script em múltiplas localizações possíveis
script_locations <- c(
  "exercicio_parte2.R", # Raiz do projeto
  "scripts/exercicio_parte2.R", # Pasta scripts
  "scripts/exercicio_parte2.R" # Alternativa
)

script_found <- FALSE
for (script_path in script_locations) {
  if (file.exists(script_path)) {
    cat("✓ Script encontrado em:", script_path, "\n")
    source(script_path)
    script_found <- TRUE
    break
  }
}

if (!script_found) {
  cat("❌ ERRO: Script de análise não encontrado\n")
  cat("📋 Locais verificados:\n")
  for (path in script_locations) {
    cat("   -", path, "\n")
  }
  cat("\n💡 SOLUÇÕES:\n")
  cat("   1. Mova 'exercicio_parte2.R' para a raiz do projeto\n")
  cat("   2. OU execute: source('scripts/exercicio_parte2.R')\n")
  stop("Script principal não localizado")
}

# RELATÓRIO FINAL DE EXECUÇÃO

cat("\n" %+% paste(rep("=", 50), collapse = "") %+% "\n")
cat("🎯 EXECUÇÃO CONCLUÍDA COM SUCESSO\n")
cat(paste(rep("=", 50), collapse = "") %+% "\n\n")

cat("📊 ARQUIVOS GERADOS:\n")

# Verificação inteligente - buscar arquivos realmente gerados
arquivos_gerados <- list.files("output", recursive = TRUE, full.names = TRUE)

if (length(arquivos_gerados) > 0) {
  cat("✅ Arquivos encontrados:\n")
  for (arquivo in arquivos_gerados) {
    tamanho <- file.size(arquivo)
    cat("  ✓", arquivo, paste0("(", round(tamanho / 1024, 1), " KB)\n"))
  }
} else {
  cat("❌ Nenhum arquivo encontrado em 'output/'\n")
}

# Verificação específica dos tipos esperados
tipos_esperados <- list(
  "CSV" = list.files("output", pattern = "\\.csv$", recursive = TRUE, full.names = TRUE), # nolint
  "PNG" = list.files("output", pattern = "\\.png$", recursive = TRUE, full.names = TRUE), # nolint
  "TXT" = list.files("output", pattern = "\\.txt$", recursive = TRUE, full.names = TRUE) # nolint
)

cat("\n📋 RESUMO POR TIPO:\n")
for (tipo in names(tipos_esperados)) {
  arquivos_tipo <- tipos_esperados[[tipo]]
  if (length(arquivos_tipo) > 0) {
    cat("✅", tipo, ":", length(arquivos_tipo), "arquivo(s)\n")
  } else {
    cat("❌", tipo, ": 0 arquivos\n")
  }
}

cat("\n📈 PRÓXIMOS PASSOS:\n")
cat("1. Revisar gráficos em 'output/graficos/'\n")
cat("2. Analisar resultados em 'output/resultados/'\n")
cat("3. Compilar relatório final\n\n")

cat("✨ Análise estatística finalizada!\n")
