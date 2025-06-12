# REQUIREMENTS.R - Instalação de Pacotes Necessários

# Lista de pacotes necessários
pacotes_necessarios <- c(
  "ggplot2",    # Gráficos
  "dplyr",      # Manipulação de dados
  "readr"       # Leitura de dados
)

# Função para instalar pacotes se não estiverem instalados
instalar_se_necessario <- function(pacote) {
  if (!require(pacote, character.only = TRUE)) {
    install.packages(pacote, dependencies = TRUE)
    library(pacote, character.only = TRUE)
  }
}

# Instalar todos os pacotes necessários
cat("Verificando e instalando pacotes necessários...\n")
lapply(pacotes_necessarios, instalar_se_necessario)

cat("Todos os pacotes foram instalados com sucesso!\n")
cat("Pacotes disponíveis:", paste(pacotes_necessarios, collapse = ", "), "\n")