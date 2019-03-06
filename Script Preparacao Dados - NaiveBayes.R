setwd("/media/n/6DEFCED45A78BAB8/workspace/Udemy/MachineLearningR/")

base <- read.csv("credit-data.csv")
base$clientid = NULL

summary(base)

idade_invalida = base[base$age < 0 | is.na(base$age), ]

# Há 4 formas nesse caso de se lidar com a data negativa e nula

# 1 matar a coluna toda
base$age = NULL

# 2 excluir da amostragem registros errados
base <- base[base$age > 0 & !is.na(base$age)]

# 3 Preencher dados manualmente para corrigir

# 4 Preencher dados com media geral, com esta excluindo indevidos
base[base$age < 0 | is.na(base$age), ] = mean(base$age[base$age > 0],na.rm = TRUE)

# Conversão de valores da classe
base$default = factor(base$default, levels = c(0,1))

base_escalonada <- base
base_escalonada[,1:3] <- scale(base_escalonada[,1:3])

# install.packages('caTools')
library(caTools)
set.seed(1)
divisao = sample.split(base_escalonada$default, SplitRatio = 0.75)
divisao
base_treinamento = subset(base_escalonada,divisao == TRUE)
base_teste = subset(base_escalonada,divisao==FALSE)


# Testes sem pré-processamento
library(caTools)
set.seed(1)
divisao = sample.split(base$default, SplitRatio = 0.75)
divisao
base_treinamento = subset(base,divisao == TRUE)
base_teste = subset(base,divisao==FALSE)

library(e1071)

classificador = naiveBayes(x = base_treinamento[,-4], y = base_treinamento$default)
print(classificador)
previsoes = predict(classificador, newdata = base_teste[,-4])
print(previsoes)

matriz_confusao = table(base_teste[,4],previsoes)
print(matriz_confusao)

install.packages('caret')
library(caret)

confusionMatrix(matriz_confusao)

# 0,9339 realizando escalonamento e tratativa de nulls
# 0,9339 realizando tratativa de nulos e faltantes
# 0,914 sem escalonamento ou tratativa de null
