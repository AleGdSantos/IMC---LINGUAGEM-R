#Trabalho Regressão Linear
#Calculo de IMC
#O conjunto refere-se aos dados de 500 pessoas, verificando o IMC com base na altura e peso.
# O Dataset poderá ser obtido em: https://www.kaggle.com/yersever/500-person-gender-height-weight-bodymassindex

#Importar dados com read.csv
dado <- read.csv("500_Person_Gender_Height_Weight_Index.csv")

# Imprimir a classe
print(class(dado))
# Verificar se dado é um data frame
print(is.data.frame(dado))
# Número de linhas
print(nrow(dado))
# Número de colunas e nomes
print(ncol(dado))
colnames(dado)



# variáveis preditoras
X <- factor((dado$Gender))#Sexo
z <- (dado$Height) #Altura
w <- (dado$Weight) #Peso

# variável de resposta 
#Indice : 0 - Extremamente fraco,1 - Fraco, 2 - Normal,3 - Excesso de peso, 4 - Obesidade, 5 - Obesidade Extrema
y <- (dado$Index) #IMC

#Cria o modelo 
modelo <- lm(y~X+z+w)
# imprime os coeficientes 
print(modelo)

# imprime o sumário 
print(summary(modelo))

#Testa a predição do modelo 
a <- data.frame(X = factor(c("Male","Female","Male","Female")), z = c(183,152,182,163), w = c(140,53,50,87))

print (a)

resultado <- as.integer(predict(modelo,a))
cat("Resultado obtido: ",resultado[[1]],"\n")
cat("Resultado obtido: ",resultado[[2]],"\n")
cat("Resultado obtido: ",resultado[[3]],"\n")
cat("Resultado obtido: ",resultado[[4]],"\n")


#Obtivemos uma boa relação entre as três variáveis preditoras(***), e uma acuracia de 82%

# Dando um nome para o arquivo da imagem.
png(file = "massa.png")
# Desenhando o gráfico
Label <- resultado
#x1 <- (z$
y1 <- a[3]
#print(dado[dado$Height >180,])
#subset(dado,Height >180)
print (levels(X))
print (table(X))