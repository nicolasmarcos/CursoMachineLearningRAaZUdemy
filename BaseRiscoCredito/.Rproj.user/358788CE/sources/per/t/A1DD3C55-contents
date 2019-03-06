setwd("/media/n/6DEFCED45A78BAB8/workspace/Udemy/MachineLearningR/BaseRiscoCredito")

base <- read.csv('risco-credito.csv')

install.packages('e1071')

library(e1071)


classificador <- naiveBayes(base[-5],base$risco)

historia = c('boa')
divida = c('alta')
garantias = c('nenhuma')
renda = c('acima_35')

teste_aprendizagem = data.frame(historia,divida,garantias,renda)

previsao = predict(classificador,newdata = teste_aprendizagem,'raw')
print(previsao)

historia = c('ruim')
divida = c('alta')
garantias = c('adequada')
renda = c('0_15')

teste_aprendizagem_laplaciana = data.frame(historia,divida,garantias,renda)

previsao = predict(classificador,newdata = teste_aprendizagem_laplaciana)
print(previsao)
