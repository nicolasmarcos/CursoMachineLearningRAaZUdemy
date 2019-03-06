setwd("/media/n/6DEFCED45A78BAB8/workspace/Udemy/MachineLearningR/BaseRiscoCredito")

base <- read.csv('risco-credito.csv')

install.packages('rpart')
install.packages('rpart.plot')

library(rpart)
library(rpart.plot)

classificador <- rpart(formula = risco ~ . ,data = base, control = rpart.control(minbucket = 1))
text(classificador)
print(classificador)
plot(classificador)

rpart.plot(classificador)

historia = c('boa','ruim')
divida = c('alta','alta')
garantias = c('nenhuma','adequada')
renda = c('acima_35','0_15' )


df_teste = data.frame(historia, divida, garantias, renda)
print(df_teste)

previsoes = predict(classificador, newdata = df_teste)

print(previsoes)
