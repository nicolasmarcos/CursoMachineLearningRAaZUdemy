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

base_escalonada <- base
base_escalonada[,1:3] <- scale(base_escalonada[,1:3])

install.packages('caTools')
library(caTools)
set.seed(1)
divisao = sample.split(base_escalonada$default, SplitRatio = 0.75)
divisao
base_treinamento = subset(base_escalonada,divisao == TRUE)
base_teste = subset(base_escalonada,divisao==FALSE)