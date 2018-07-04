# == fazendo analise com Rede Neural Multilayer Perception ==

#instalando o pacote para Rede Neual Multilayer Perception
install.packages("neuralnet")
library(neuralnet)

# criar um cópia para não mecher no original
myiris = iris

#binarizar o arquivo myiris
# binarizando e criando uma coluna para quando for setosa
myiris = cbind(myiris, myiris$Species =='setosa')

#verificando se deu certo ...
head(myiris)
tail(myiris)

# Fazendo para versicolor ....
myiris = cbind(myiris, myiris$Species =='versicolor')

# Fazendo para virginica ....
myiris = cbind(myiris, myiris$Species =='virginica')

#verificando se as colunas fizeram certas
summary(myiris)

#renomeando as colunas criadas para binarização
#usando a função names
names(myiris)[6] = 'setosa'
names(myiris)[7] = 'versicolor'
names(myiris)[8] = 'virginica'

#verificando se as colunas foram renomeadas corretamente
summary(myiris)

#dividir esse conjunto de dados para train e test
amostra = sample(2, 150, replace=T, prob = c(0.7,0.3))
myiristreino = myiris[amostra == 1,]
myiristeste = myiris[amostra == 2,]

#verificar as dimensões dos arquivos criados
dim(myiristreino)
dim(myiristeste)

#criar o modelo... especficando que eu vou usar o hidden (5,4) para criar camadas com cinco neuronios e outra com 4 neuronios
modelo = neuralnet(setosa + versicolor + virginica ~Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, myiristreino, hidden=c(5,4))

# verificando o modelo criado
print(modelo)

#plotando para ver o modelo
plot(modelo)

#testar o modelo usando a função compute
teste = compute(modelo, myiristeste[,1:4])

# para verificar a previsão
teste$net.result

#criar um dataframe para os resultados
resultado = as.data.frame(teste$net.result)

# renomear essas colunas... usando a função names
names(resultado)[1] = 'setosa'
names(resultado)[2] = 'versicolor'
names(resultado)[3] = 'virginica'

#verificando o dataframe
head(resultado)

#criar uma coluna classe para verificar qual é o maior peso para determinar o tipo.
#usando a função max.col e o method ties
resultado$class = colnames(resultado[,1:3])[max.col(resultado[,1:3],ties.method='first')]

#verificando o dataframe novamente
head(resultado)
resultado

#criando a matrix de confsão
confusao = table(resultado$class,myiristeste$Species)

#verificar os acertos usando uma soma diagonal
sum(diag(confusao) * 100 / sum(confusao))
confusao
