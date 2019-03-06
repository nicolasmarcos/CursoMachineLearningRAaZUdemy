# Librarys necessárias
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)

setwd('/media/n/6DEFCED45A78BAB8/workspace/Udemy/MachineLearningR')


# cenário 1 - Tratativa de nulos, inconsistentes e escalonamento

base = read.csv('credit-data.csv')
base$clientid = NULL
summary(base)

idade_invalida = base[base$age < 0 & !is.na(base$age), ]

# 3 preencher os dados manualmente

# calcular a m?dia da idade
mean(base$age, na.rm = TRUE)
mean(base$age[base$age > 0], na.rm = TRUE)
base$age = ifelse(base$age < 0, 40.92, base$age)

base[is.na(base$age), ]
base$age = ifelse(is.na(base$age), mean(base$age, na.rm = TRUE), base$age)

base[, 1:3] = scale(base[, 1:3])

# Encode da classe
base$default = factor(base$default, levels = c(0,1))

set.seed(1)
divisao = sample.split(base$default, SplitRatio = 0.75)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

classificador = rpart(formula = default ~ ., data = base_treinamento)
print(classificador)
rpart.plot(classificador)

previsoes = predict(classificador, newdata = base_teste[-4], type = 'class')

matriz_confusao = table(base_teste[,4], previsoes)
confusionMatrix(matriz_confusao)

# cenário 2 - Tratativa de nulos e inconsistentes

base = read.csv('credit-data.csv')
base$clientid = NULL
summary(base)

idade_invalida = base[base$age < 0 & !is.na(base$age), ]

# 3 preencher os dados manualmente

# calcular a m?dia da idade
mean(base$age, na.rm = TRUE)
mean(base$age[base$age > 0], na.rm = TRUE)
base$age = ifelse(base$age < 0, 40.92, base$age)

base[is.na(base$age), ]
base$age = ifelse(is.na(base$age), mean(base$age, na.rm = TRUE), base$age)

# Encode da classe
base$default = factor(base$default, levels = c(0,1))

set.seed(1)
divisao = sample.split(base$default, SplitRatio = 0.75)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

classificador = rpart(formula = default ~ ., data = base_treinamento)
print(classificador)
rpart.plot(classificador)

previsoes = predict(classificador, newdata = base_teste[-4], type = 'class')

matriz_confusao = table(base_teste[,4], previsoes)
confusionMatrix(matriz_confusao)

# cenário 3 - Sem tratativas de pré-procesamento

base = read.csv('credit-data.csv')
base$clientid = NULL


# 3 preencher os dados manualmente

# Encode da classe
base$default = factor(base$default, levels = c(0,1))

set.seed(1)
divisao = sample.split(base$default, SplitRatio = 0.75)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

classificador = rpart(formula = default ~ ., data = base_treinamento)
print(classificador)
rpart.plot(classificador)

previsoes = predict(classificador, newdata = base_teste[-4], type = 'class')

matriz_confusao = table(base_teste[,4], previsoes)
confusionMatrix(matriz_confusao)

# Resultados Teste por Cenário:

# cenário 1 - Tratativa de nulos, inconsistentes e escalonamento
# Accuracy : 0.97  

# cenário 2 - Tratativa de nulos e inconsistentes 
# Accuracy : 0.97  

# cenário 3 - Sem tratativas de pré-procesamento
# Accuracy : 0.97  