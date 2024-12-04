# Carregar bibliotecas
library(readxl)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(corrplot)
library(pheatmap)
library(reshape2)

# Ler os dados

dataset <- read_excel("Passanger_Satisfaction_survey_dataset_LR.xlsx", sheet="Satisfaction")

# Inspecionar os dados
str(dataset)  
summary(dataset)  

# Remover dados ausentes ('Arrival Delay in Minutes')
dataset <- dataset %>%
  filter(!is.na(`Arrival Delay in Minutes`))

# Remover a coluna "satisfaction_v1" 
dataset <- dataset %>%
  select(-satisfaction_v1)

# Codificar variáveis categóricas para numéricas
dataset <- dataset %>%
  mutate(
    Gender = ifelse(Gender == "Male", 1, 0),
    `Customer Type` = ifelse(`Customer Type` == "Loyal Customer", 1, 0),
    `Type of Travel` = ifelse(`Type of Travel` == "Business travel", 1, 0),
    Class = case_when(
      Class == "Business" ~ 2,
      Class == "Eco Plus" ~ 1,
      TRUE ~ 0
    ),
    satisfaction_v2 = ifelse(satisfaction_v2 == "satisfied", 1, 0)
  )

# Calcular a matriz de correlação
cor_matrix <- cor(dataset, use = "complete.obs")  # Ignorar valores NA
# Usando ggplot2
cor_melt <- melt(cor_matrix)  
ggplot(data = cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "green", mid = "white", midpoint = 0,
                       limits = c(-1, 1),    
                       name = "Correlação") + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    axis.title.x = element_blank(),                    
    axis.title.y = element_blank(),                    
    legend.position = "right"                          
  ) +
  labs(
    title = "Mapa de Calor das Correlações", 
    fill = "Correlação"
  )

# Normalizar variáveis numéricas entre 0 e 1  #Em algoritmos como gradiente descendente (usado em redes neurais e regressão), grandes diferenças de escala podem fazer com que o modelo leve mais tempo para encontrar a solução ótima.
dataset <- dataset %>%
  mutate(across(where(is.numeric), ~ (.-min(.))/(max(.)-min(.)), .names = "normalized_{.col}")) %>%
  select(-normalized_id)

# Ordenar os dados por 'id'
dataset <- dataset %>%
  arrange(id)

# Salvar o dataset limpo
write.csv(dataset, "Cleaned_Satisfaction_binomial")

# Separar os dados em treino (80%) e teste (20%)

# Definir proporção de treino
set.seed(123)  # Para garantir reprodutibilidade (resultados da amostragem sejam sempre os mesmos)
train_indices <- sample(seq_len(nrow(dataset)), size = 0.8 * nrow(dataset))

# Dividir o dataset
train_data <- dataset[train_indices, ]  # 80% para treino
test_data <- dataset[-train_indices, ]  # 20% para teste

# Verificar os tamanhos dos conjuntos
cat("Tamanho do conjunto de treino:", nrow(train_data), "\n")
cat("Tamanho do conjunto de teste:", nrow(test_data), "\n")

# Salvar os conjuntos de treino e teste
write.csv(train_data, "Train_Passenger_Satisfaction_binomial.csv", row.names = FALSE)
write.csv(test_data, "Test_Satisfaction_binomial.csv", row.names = FALSE)

