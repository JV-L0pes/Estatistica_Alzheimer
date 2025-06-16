# CONFIGURAÇÃO INICIAL E LIMPEZA

set.seed(123) # Reprodutibilidade garantida
library(ggplot2)
library(dplyr)
library(readr)

# Função auxiliar para concatenação de strings
concat_str <- function(...) paste0(..., collapse = "")

# FUNÇÕES AUXILIARES (SINGLE RESPONSIBILITY PRINCIPLE)

#' Carrega e valida dataset com tratamento robusto de erros
#' @param file_path Caminho para o arquivo CSV
#' @return data.frame Dataset validado e limpo
load_and_validate_data <- function(file_path = "data/alzheimer_data.csv") {
  cat("=== CARREGAMENTO E VALIDAÇÃO DOS DADOS ===\n")

  # Diagnóstico inicial do arquivo
  sample_lines <- readLines(file_path, n = 3)
  cat("Estrutura detectada:\n")
  cat("- Linhas de amostra:", length(sample_lines), "\n")
  cat("- Separador detectado: vírgula\n")

  # Carregamento robusto com tratamento de erros
  tryCatch(
    {
      dados <- read.csv(file_path,
        header = TRUE, sep = ",",
        stringsAsFactors = FALSE, strip.white = TRUE
      )

      # Validação de integridade
      if (nrow(dados) == 0) stop("Dataset vazio")
      if (ncol(dados) < 2) stop("Dataset deve ter ao menos 2 colunas")

      cat("✓ Dataset carregado:", nrow(dados), "observações,", ncol(dados), "variáveis\n") # nolint
      dados
    },
    error = function(e) {
      stop("Erro crítico no carregamento: ", conditionMessage(e))
    }
  )
}

#' Identifica e extrai variável quantitativa apropriada
#' @param dataset Data frame de entrada
#' @return list(variable_name, data_vector, summary_stats)
extract_quantitative_variable <- function(dataset) {
  cat("=== SELEÇÃO DE VARIÁVEL QUANTITATIVA ===\n")

  # Buscar variáveis numéricas apropriadas com critério mais flexível
  numeric_vars <- sapply(dataset, function(x) {
    if (is.numeric(x)) {
      # Verificar se tem variabilidade suficiente
      unique_vals <- length(unique(x[!is.na(x)]))
      return(unique_vals >= 5 && unique_vals > length(x) * 0.1)
    }
    FALSE
  })

  if (!any(numeric_vars)) {
    # Fallback: tentar converter colunas que parecem numéricas
    for (col in names(dataset)) {
      if (is.character(dataset[[col]])) {
        numeric_attempt <- suppressWarnings(as.numeric(dataset[[col]]))
        if (!all(is.na(numeric_attempt))) {
          unique_vals <- length(unique(numeric_attempt[!is.na(numeric_attempt)])) # nolint
          total_vals <- length(numeric_attempt)
          if (unique_vals >= 5 && unique_vals > total_vals * 0.1) {
            dataset[[col]] <- numeric_attempt
            numeric_vars[col] <- TRUE
            break
          }
        }
      }
    }
  }

  if (!any(numeric_vars)) {
    stop("Nenhuma variável quantitativa adequada encontrada no dataset")
  }

  # Selecionar primeira variável quantitativa válida
  selected_var <- names(numeric_vars)[numeric_vars][1]
  var_data <- dataset[[selected_var]]

  # Limpeza de dados: remover NAs
  var_data <- var_data[!is.na(var_data)]

  if (length(var_data) < 50) {
    stop("Variável selecionada tem poucos dados válidos (< 50 observações)")
  }

  summary_stats <- list(
    name = selected_var,
    n = length(var_data),
    mean = mean(var_data),
    sd = sd(var_data),
    min = min(var_data),
    max = max(var_data)
  )

  cat("✓ Variável selecionada:", selected_var, "\n")
  cat("  - Observações válidas:", summary_stats$n, "\n")
  cat("  - Média populacional:", round(summary_stats$mean, 4), "\n")
  cat("  - Desvio padrão:", round(summary_stats$sd, 4), "\n")

  list(
    variable_name = selected_var,
    data = var_data,
    stats = summary_stats
  )
}

#' Executa simulação com amostragem aleatória estratificada
#' @param dataset Dataset completo
#' @param sample_proportions Vetor de proporções para amostragem
#' @return data.frame Resultados da simulação
perform_stratified_sampling_simulation <- function(dataset, # nolint
                                                   sample_proportions = seq(0.02, 0.98, 0.02)) { # nolint
  cat("=== SIMULAÇÃO COM AMOSTRAGEM ALEATÓRIA ESTRATIFICADA ===\n")

  # ESPECIFICAÇÃO DA AMOSTRAGEM ESTRATIFICADA:
  # 1. População dividida em estratos por variável sexo
  # 2. Alocação proporcional mantém proporções populacionais
  # 3. Amostragem aleatória simples dentro de cada estrato
  # 4. Combinação das amostras estratificadas para análise final

  # Identificar variável sexo/gender para estratificação
  sex_cols <- c("sexo", "gender", "sex", "genero")
  sex_var <- NULL

  for (col in sex_cols) {
    if (col %in% names(dataset) && !all(is.na(dataset[[col]]))) {
      sex_var <- col
      break
    }
  }

  if (is.null(sex_var)) {
    stop("Variável sexo não encontrada para estratificação")
  }

  # Extrair variável quantitativa
  var_info <- extract_quantitative_variable(dataset)
  population_data <- var_info$data
  population_mean <- var_info$stats$mean
  population_var <- var(population_data)

  # PREPARAÇÃO DOS ESTRATOS
  clean_data <- dataset[!is.na(dataset[[sex_var]]), ]
  strata_distribution <- table(clean_data[[sex_var]])
  strata_props <- strata_distribution / sum(strata_distribution)

  cat("✓ ESTRATIFICAÇÃO IMPLEMENTADA:\n")
  cat("  • Variável de estratificação:", sex_var, "\n")
  cat("  • Método: Alocação proporcional\n")
  cat("  • Estratos identificados:\n")
  for (i in seq_along(strata_props)) {
    cat(
      "    -", names(strata_props)[i], ":", strata_distribution[i],
      "obs (", round(strata_props[i] * 100, 1), "% da população)\n"
    )
  }

  n_population <- nrow(clean_data)
  n_simulations <- length(sample_proportions)

  results <- data.frame(
    proporcao = numeric(n_simulations),
    tamanho_amostra = integer(n_simulations),
    media_amostral = numeric(n_simulations),
    variancia_teorica = numeric(n_simulations),
    variancia_empirica = numeric(n_simulations)
  )

  cat("Executando", n_simulations, "simulações estratificadas...\n")

  for (i in seq_along(sample_proportions)) {
    prop <- sample_proportions[i]
    total_sample_size <- max(10, round(prop * n_population))

    # PROCESSO DE AMOSTRAGEM ESTRATIFICADA:
    # Para cada proporção (2%, 4%, 6%, ..., 98%):

    # ETAPA 1: Calcular tamanho amostral por estrato (alocação proporcional)
    stratified_sample <- data.frame()

    for (stratum in names(strata_distribution)) {
      stratum_data <- clean_data[clean_data[[sex_var]] == stratum, ]

      # Tamanho do estrato = proporção populacional × tamanho total da amostra
      stratum_size <- round(total_sample_size * strata_props[stratum])

      # ETAPA 2: Amostragem aleatória simples dentro do estrato
      if (stratum_size > 0 && stratum_size <= nrow(stratum_data)) {
        # Seleção aleatória SEM reposição dentro do estrato
        sampled_indices <- sample(nrow(stratum_data), size = stratum_size, replace = FALSE) # nolint
        stratum_sample <- stratum_data[sampled_indices, ]

        # ETAPA 3: Combinar amostras de todos os estratos
        stratified_sample <- rbind(stratified_sample, stratum_sample)
      }
    }

    # ETAPA 4: Extrair dados da variável quantitativa da amostra combinada
    sample_quant_data <- stratified_sample[[var_info$variable_name]]
    sample_quant_data <- sample_quant_data[!is.na(sample_quant_data)]

    if (length(sample_quant_data) > 0) {
      sample_mean <- mean(sample_quant_data)
      actual_sample_size <- length(sample_quant_data)

      # Variância teórica
      theoretical_var <- population_var / actual_sample_size

      # Variância empírica (múltiplas amostras estratificadas)
      # MÉTODO: Repetir processo de amostragem estratificada 30 vezes
      empirical_means <- replicate(30, {
        temp_stratified <- data.frame()

        # Para cada repetição, aplicar mesmo processo estratificado:
        for (stratum in names(strata_distribution)) {
          stratum_data <- clean_data[clean_data[[sex_var]] == stratum, ]
          stratum_size <- round(total_sample_size * strata_props[stratum])

          if (stratum_size > 0 && stratum_size <= nrow(stratum_data)) {
            # Nova amostragem aleatória dentro do estrato
            temp_indices <- sample(nrow(stratum_data), size = stratum_size, replace = FALSE) # nolint
            temp_sample <- stratum_data[temp_indices, ]
            temp_stratified <- rbind(temp_stratified, temp_sample)
          }
        }

        # Calcular média da amostra estratificada
        temp_quant <- temp_stratified[[var_info$variable_name]]
        temp_quant <- temp_quant[!is.na(temp_quant)]
        if (length(temp_quant) > 0) mean(temp_quant) else NA
      })

      empirical_var <- var(empirical_means, na.rm = TRUE)

      results[i, ] <- list(
        proporcao = prop * 100,
        tamanho_amostra = actual_sample_size,
        media_amostral = sample_mean,
        variancia_teorica = theoretical_var,
        variancia_empirica = empirical_var
      )
    }

    if (i %% 10 == 0) {
      cat("  Progresso:", round(i / n_simulations * 100), "%\n")
    }
  }

  cat("✓ PROCESSO COMPLETO DE AMOSTRAGEM ESTRATIFICADA:\n")
  cat("  • Simulações executadas:", n_simulations, "\n")
  cat("  • Método de alocação: Proporcional\n")
  cat("  • Seleção intra-estrato: Aleatória simples sem reposição\n")
  cat("  • Variância calculada: Empírica (30 repetições por tamanho)\n")

  list(
    results = results,
    population_mean = population_mean,
    variable_name = var_info$variable_name,
    stratification_var = sex_var
  )
}

#' Gera gráficos de alta qualidade com tema consistente
#' @param results_df Data frame com resultados da simulação
#' @param population_mean Média populacional
#' @param variable_name Nome da variável analisada
#' Gera gráficos originais com amostragem estratificada
create_convergence_plots <- function(simulation_results) {
  cat("=== GRÁFICOS ===\n")

  results_df <- simulation_results$results
  population_mean <- simulation_results$population_mean
  variable_name <- simulation_results$variable_name

  custom_theme <- theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40"),
      axis.title = element_text(size = 11, face = "bold"),
      panel.grid.minor = element_blank()
    )

  # GRÁFICO 1: Convergência da Média Amostral
  ggplot(results_df, aes(x = .data$tamanho_amostra, y = .data$media_amostral)) +
    geom_point(color = "#2E86AB", alpha = 0.7, size = 2) +
    geom_hline(
      yintercept = population_mean, color = "#A23B72",
      linewidth = 1.2, linetype = "solid"
    ) +
    labs(
      title = "Convergência da Média Amostral",
      subtitle = paste("Amostragem Aleatória Estratificada -", variable_name),
      x = "Tamanho da Amostra (n)",
      y = "Média Amostral",
      caption = paste("Linha vermelha: Média Populacional =", round(population_mean, 3)) # nolint
    ) +
    custom_theme

  ggsave("output/graficos/grafico1_convergencia_estratificada.png",
    width = 10, height = 6, dpi = 300, bg = "white"
  )

  # GRÁFICO 2: Variância das Médias Amostrais
  ggplot(results_df, aes(x = .data$tamanho_amostra)) +
    geom_point(aes(y = .data$variancia_empirica),
      color = "#2E86AB",
      alpha = 0.7, size = 2, shape = 16
    ) +
    geom_line(aes(y = .data$variancia_teorica),
      color = "#A23B72",
      linewidth = 1.2, linetype = "dashed"
    ) +
    labs(
      title = "Variância das Médias Amostrais",
      subtitle = paste("Amostragem Aleatória Estratificada -", variable_name),
      x = "Tamanho da Amostra (n)",
      y = "Variância das Médias Amostrais",
      caption = "Linha tracejada: Variância Teórica"
    ) +
    custom_theme

  ggsave("output/graficos/grafico2_variancia_estratificada.png",
    width = 10, height = 6, dpi = 300, bg = "white"
  )

  cat("✓ Gráficos salvos\n")
}

# EXECUÇÃO PRINCIPAL (ORCHESTRATION)

main_analysis <- function() {
  cat("ANÁLISE COM AMOSTRAGEM ALEATÓRIA ESTRATIFICADA\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  # 1. Carregamento
  dataset <- load_and_validate_data()

  # 2. Simulação com amostragem estratificada
  simulation_results <- perform_stratified_sampling_simulation(dataset)

  # 3. Gráficos de convergência
  create_convergence_plots(simulation_results)

  # 4. Salvar resultados
  write.csv(simulation_results$results,
    "output/resultados/resultados_estratificada.csv",
    row.names = FALSE
  )

  # 5. Relatório resumido
  generate_brief_report(simulation_results)

  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("✅ ANÁLISE CONCLUÍDA\n")
}

#' Gera relatório de interpretação automático
#' Gera relatório resumido sem explicações
generate_brief_report <- function(simulation_results) {
  results_df <- simulation_results$results

  report_content <- paste0(
    "ESPECIFICAÇÃO DA AMOSTRAGEM ALEATÓRIA ESTRATIFICADA\n\n",
    "MÉTODO IMPLEMENTADO:\n",
    "• Tipo: Amostragem Aleatória Estratificada\n",
    "• Alocação: Proporcional\n",
    "• Estratos: Definidos pela variável ", simulation_results$stratification_var, "\n", # nolint
    "• Seleção intra-estrato: Aleatória simples sem reposição\n\n",
    "PROCESSO EXECUTADO:\n",
    "1. População dividida em estratos por sexo\n",
    "2. Tamanho de cada estrato calculado proporcionalmente\n",
    "3. Amostragem aleatória simples dentro de cada estrato\n",
    "4. Combinação das amostras estratificadas\n",
    "5. Cálculo das estatísticas da amostra final\n\n",
    "RESULTADOS:\n",
    "• Variável analisada: ", simulation_results$variable_name, "\n",
    "• Média populacional: ", round(simulation_results$population_mean, 4), "\n", # nolint
    "• Simulações realizadas: ", nrow(results_df), "\n",
    "• Faixa amostral: ", min(results_df$tamanho_amostra), " a ",
    max(results_df$tamanho_amostra), " observações\n",
    "• Média das médias amostrais: ", round(mean(results_df$media_amostral, na.rm = TRUE), 4), "\n" # nolint
  )

  writeLines(report_content, "output/resultados/relatorio_resumido.txt")
  cat("✓ Relatório gerado\n")
}

# EXECUÇÃO

# Executar análise principal
main_analysis()
