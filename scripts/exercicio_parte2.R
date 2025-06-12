# Configuração inicial
rm(list = ls())  # Limpar environment
set.seed(123)    # Reprodutibilidade

# Carregar bibliotecas
library(ggplot2)
library(dplyr)
library(readr)

# 1. CARREGAMENTO E PREPARAÇÃO DOS DADOS

# Diagnóstico do arquivo primeiro
cat("=== DIAGNÓSTICO DO ARQUIVO ===\n")
linhas_teste <- readLines("data/alzheimer_data.csv", n = 3)
cat("Primeiras 3 linhas:\n")
for (i in seq_along(linhas_teste)) {
  cat("Linha", i, ":", substr(linhas_teste[i], 1, 100), "...\n")
}

# Tentar diferentes métodos de leitura
cat("\n=== TENTANDO DIFERENTES SEPARADORES ===\n")

# Carregar dataset (sem header, baseado na amostra fornecida)
dados <- read.table("data/alzheimer_data.csv", header = TRUE, sep = ",", dec = ".", strip.white = TRUE, quote = "\"") # nolint

# Verificar estrutura
print(paste(
  "Número de observações:", nrow(dados)
))
print(paste(
  "Número de variáveis:", ncol(dados)
))

# Definir nomes das colunas dinamicamente baseado no número real de colunas
if (ncol(dados) >= 2) {
  nomes_base <- c("id", "age", "gender", "education", "ses", "mmse", "cdr",  # nolint
                  "etiv", "nwbv", "asf", "delay", "m_f", "hand", "group")
   # nolint # nolint
  if (ncol(dados) <= length(nomes_base)) {
    colnames(dados) <- nomes_base[seq_len(ncol(dados))]
  } else {
    # Mais colunas que nomes predefinidos
    nomes_extras <- paste0("var", (length(nomes_base) + 1):ncol(dados))
    colnames(dados) <- c(nomes_base, nomes_extras)
  }
} else {
  stop("Dataset tem apenas 1 coluna. Verifique o formato do arquivo.")
}

# Mostrar primeiras linhas
cat("\n=== ESTRUTURA DOS DADOS ===\n")
print(head(dados, 3))
cat("Nomes das colunas:", paste(colnames(dados), collapse = ", "), "\n")

# 2. SELEÇÃO DA VARIÁVEL QUANTITATIVA

# Variável escolhida: AGE (segunda coluna)
variavel_escolhida <- "age"

# Verificar se variável existe e extrair dados
if (!variavel_escolhida %in% colnames(dados)) {
  stop("Variável não encontrada no dataset!")
}

# Extrair dados da variável (idade está na segunda coluna)
dados_variavel <- dados[[variavel_escolhida]]

# Verificar se são dados numéricos
if (!is.numeric(dados_variavel)) {
  dados_variavel <- as.numeric(dados_variavel)
}

# Remover valores NA se existirem
dados_variavel <- dados_variavel[!is.na(dados_variavel)]
n_populacional <- length(dados_variavel)

print(paste("Variável selecionada:", variavel_escolhida))
print(paste("Tamanho populacional (sem NAs):", n_populacional))
print(paste("Faixa de valores:", min(dados_variavel), "a", max(dados_variavel)))

# 3. CÁLCULO DA MÉDIA POPULACIONAL

media_populacional <- mean(dados_variavel)
print(paste("Média populacional:", round(media_populacional, 4)))

# 4. PREPARAÇÃO DOS VETORES DE RESULTADOS

# Criar vetores vazios
media_amostral <- numeric()
variancia_media_amostral <- numeric()
tamanhos_amostrais <- numeric()

# 5. LOOP DE AMOSTRAGEM (2%, 4%, 6%, ..., 98%)

print("Iniciando loop de amostragem...")

# Proporções: 2% até 98% (passo de 2%)
proporcoes <- seq(0.02, 0.98, by = 0.02)

for (i in seq_along(proporcoes)) {
  prop <- proporcoes[i]
   # nolint
  # Calcular tamanho da amostra
  n_amostra <- round(prop * n_populacional)
   # nolint # nolint
  # Evitar amostras muito pequenas
  if (n_amostra < 2) n_amostra <- 2
   # nolint # nolint
  # Realizar Amostragem Aleatória Simples
  indices_amostra <- sample(seq_len(n_populacional), size = n_amostra, replace = FALSE) # nolint # nolint
  amostra <- dados_variavel[indices_amostra]
   # nolint # nolint
  # Calcular média amostral
  media_atual <- mean(amostra)
   # nolint # nolint
  # Para variância das médias amostrais, usamos múltiplas amostras
  # (simulação para demonstrar propriedades teóricas)
  n_simulacoes <- 100
  medias_simulacao <- numeric(n_simulacoes)
   # nolint
  for (j in seq_len(n_simulacoes)) {
    indices_sim <- sample(seq_len(n_populacional), size = n_amostra, replace = FALSE) # nolint # nolint
    amostra_sim <- dados_variavel[indices_sim]
    medias_simulacao[j] <- mean(amostra_sim)
  }
   # nolint # nolint
  # Calcular variância das médias amostrais
  variancia_atual <- var(medias_simulacao)
   # nolint # nolint
  # Armazenar resultados
  tamanhos_amostrais <- c(tamanhos_amostrais, n_amostra)
  media_amostral <- c(media_amostral, media_atual)
  variancia_media_amostral <- c(variancia_media_amostral, variancia_atual)
   # nolint
  # Progress indicator
  if (i %% 10 == 0) {
    print(paste("Processando:", round(prop * 100), "% da população"))
  }
}

print("Loop de amostragem concluído!")

# 6. CRIAR DATAFRAME DOS RESULTADOS

resultados <- data.frame(
  proporcao = proporcoes * 100,
  tamanho_amostra = tamanhos_amostrais,
  media_amostral = media_amostral,
  variancia_media_amostral = variancia_media_amostral
)

# Salvar resultados
write.csv(resultados, "output/resultados/resultados_amostragem.csv", row.names = FALSE) # nolint

# 7. GRÁFICO 1: MÉDIAS AMOSTRAIS VS TAMANHOS AMOSTRAIS

grafico1 <- ggplot(resultados, aes(x = tamanho_amostra, y = media_amostral)) +
  geom_point(color = "blue", alpha = 0.7, size = 2) +
  geom_hline(yintercept = media_populacional, color = "red", linewidth = 1.2,  # nolint # nolint
             linetype = "solid") +
  labs(
    title = paste("Convergência da Média Amostral para Média Populacional"),
    subtitle = paste("Variável:", variavel_escolhida),
    x = "Tamanho da Amostra (n)",
    y = "Média Amostral",
    caption = paste("Linha vermelha: Média Populacional =",  # nolint
                    round(media_populacional, 4))
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 11),
    legend.position = "none"
  )

print(grafico1)

# Salvar gráfico
ggsave(
  "output/graficos/grafico1_medias_amostrais.png",  # nolint
  grafico1, width = 10, height = 6, dpi = 300, bg = "white"
)

# 8. GRÁFICO 2: VARIÂNCIA DAS MÉDIAS VS TAMANHOS AMOSTRAIS

grafico2 <- ggplot(resultados, aes(x = tamanho_amostra, y = sqrt(variancia_media_amostral))) + # nolint
  geom_point(color = "darkgreen", alpha = 0.7, size = 2) +
  geom_hline(yintercept = 0, color = "purple", linewidth = 1.2, linetype = "solid") +
  labs(
    title = "Variância das Médias Amostrais vs Tamanho da Amostra",
    subtitle = paste("Variável:", variavel_escolhida),
    x = "Tamanho da Amostra (n)",
    y = "Variância das Médias Amostrais",
    caption = "Linha roxa: y = 0 (referência)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 11),
    legend.position = "none"
  ) +
  # Ajustar eixo y para tornar linha roxa visível
  ylim(0, max(resultados$variancia_media_amostral) * 1.1)

print(grafico2)

# Salvar gráfico
ggsave(
  "output/graficos/grafico2_variancias_medias.png",  # nolint
  grafico2, width = 10, height = 6, dpi = 300, bg = "white"
)

# 9. RESUMO ESTATÍSTICO

cat("\n=== RESUMO DOS RESULTADOS ===\n")
cat("Variável analisada:", variavel_escolhida, "\n")
cat("Tamanho populacional:", n_populacional, "\n")
cat("Média populacional:", round(media_populacional, 4), "\n")
cat("Número de amostras analisadas:", length(proporcoes), "\n")
cat("Faixa de tamanhos amostrais:", min(tamanhos_amostrais), "a", max(tamanhos_amostrais), "\n") # nolint
cat("Média das médias amostrais:", round(mean(media_amostral), 4), "\n")
cat("Desvio padrão das médias amostrais:", round(sd(media_amostral), 4), "\n")

# 10. INTERPRETAÇÃO DOS GRÁFICOS

interpretacao <- paste0(
  "=== INTERPRETAÇÃO DOS GRÁFICOS ===\n\n",
   # nolint # nolint
  "GRÁFICO 1 - Convergência da Média Amostral:\n",
  "• As médias amostrais oscilam em torno da média populacional (linha vermelha).\n", # nolint
  "• À medida que o tamanho da amostra aumenta, as médias amostrais tendem a se aproximar da média populacional.\n", # nolint
  "• Isso demonstra a propriedade de que a média amostral é um estimador não-viesado da média populacional.\n", # nolint
  "• A variabilidade das médias amostrais diminui com o aumento do tamanho da amostra.\n\n", # nolint # nolint
   # nolint
  "GRÁFICO 2 - Variância das Médias Amostrais:\n",
  "• A variância das médias amostrais diminui à medida que o tamanho da amostra aumenta.\n", # nolint
  "• Isso ilustra que amostras maiores produzem estimativas mais precisas da média populacional.\n", # nolint
  "• A tendência decrescente confirma o Teorema do Limite Central: Var(X̄) = σ²/n.\n", # nolint # nolint
  "• A linha roxa (y=0) serve como referência, mostrando que a variância se aproxima de zero para amostras muito grandes.\n\n", # nolint # nolint
   # nolint
  "CONCLUSÃO:\n",
  "Os gráficos demonstram duas propriedades fundamentais da inferência estatística:\n", # nolint
  "1. Consistência: À medida que n aumenta, as médias amostrais convergem para a média populacional.\n", # nolint
  "2. Eficiência: Amostras maiores resultam em estimativas mais precisas (menor variabilidade).\n" # nolint
)

cat(interpretacao)

# Salvar interpretação
writeLines(interpretacao, "output/resultados/interpretacao_graficos.txt")

cat("\n=== ARQUIVOS GERADOS ===\n")
cat("• Resultados: output/resultados/resultados_amostragem.csv\n")
cat("• Gráfico 1: output/graficos/grafico1_medias_amostrais.png\n")
cat("• Gráfico 2: output/graficos/grafico2_variancias_medias.png\n")
cat("• Interpretação: output/resultados/interpretacao_graficos.txt\n")
cat("\nExercício da Parte 2 concluído com sucesso!\n")