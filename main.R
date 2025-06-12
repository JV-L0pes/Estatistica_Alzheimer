# MAIN.R - Arquivo Principal de Execução

# Limpar ambiente
rm(list = ls())

cat("=== TRABALHO DE ESTATÍSTICA - PARTE 2 ===\n")
cat("Exercício: Tamanho Amostral e Propriedades da Média Amostral\n\n")


# 1. CRIAR ESTRUTURA DE PASTAS

cat("1. Criando estrutura de pastas...\n")

# Criar pastas se não existirem
if (!dir.exists("data")) dir.create("data")
if (!dir.exists("scripts")) dir.create("scripts")
if (!dir.exists("output")) dir.create("output")
if (!dir.exists("output/graficos")) dir.create("output/graficos")
if (!dir.exists("output/resultados")) dir.create("output/resultados")

cat("   ✓ Estrutura de pastas criada\n\n")

# 2. INSTALAR DEPENDÊNCIAS

cat("2. Verificando dependências...\n")
source("requirements.R")
cat("   ✓ Dependências verificadas\n\n")

# 3. EXECUTAR EXERCÍCIO PRINCIPAL

cat("3. Executando exercício da Parte 2...\n")

# Verificar se arquivo de dados existe
if (!file.exists("data/alzheimer_data.csv")) {
  cat("   ⚠️  ATENÇÃO: Arquivo 'data/alzheimer_data.csv' não encontrado!\n")
  cat("   📋 INSTRUÇÕES:\n")
  cat("      1. Baixe seu dataset do GitHub/Google Drive\n")
  cat("      2. Salve como 'alzheimer_data.csv' na pasta 'data/'\n")
  cat("      3. Execute novamente: source('main.R')\n\n")
  cat("   📁 Estrutura esperada:\n")
  cat("      trabalho-alzheimer-parte2/\n")
  cat("      ├── data/\n")
  cat("      │   └── alzheimer_data.csv  ← SEU ARQUIVO AQUI\n")
  cat("      ├── main.R\n")
  cat("      └── ...\n\n")
  stop("Dataset não encontrado. Siga as instruções acima.")
}

# Executar exercício principal
source("scripts/exercicio_parte2.R")

cat("\n=== EXECUÇÃO CONCLUÍDA ===\n")
cat("✅ Todos os arquivos foram gerados com sucesso!\n\n")

cat("📊 ARQUIVOS GERADOS:\n")
cat("• output/resultados/resultados_amostragem.csv\n")
cat("• output/graficos/grafico1_medias_amostrais.png\n")
cat("• output/graficos/grafico2_variancias_medias.png\n")
cat("• output/resultados/interpretacao_graficos.txt\n\n")

cat("📋 PRÓXIMOS PASSOS:\n")
cat("1. Verifique os gráficos gerados\n")
cat("2. Leia a interpretação em 'interpretacao_graficos.txt'\n")
cat("3. Compile os resultados para entrega\n\n")

cat("✨ Exercício da Parte 2 concluído!\n")