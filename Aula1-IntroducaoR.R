#Script da aula 1 - Introdução ao R. 

#Exercício 01 - Básica do R

library(datasets) #Comando para importacao de pacotes.

mtcars #base de dados contida no pacote datasets

base <- mtcars #comando para atribuir a base de dados do pacote datasets a um objeto

# Em R um objeto é un elemento definido dentro de uma sessão R. Podem ser: base de dados, vetores, nomes, matrizes, listas, resultados de regressao, graficos, funcoes, mapas..

View(base) #visualizar uma base de dados. 

head(base) #comando para visualizar uma base de dados de forma resumida, apenas as primeiras entradas da base.

head(base, 
     n=12) #boa prática em R é realizar quebras de linha quando há muitas opções em um único comando.

rm(base) # rm remove qualquer objeto em R

2+3

2*3

2^3

Y <- 2+3
Y

#Classes de objetos
num <- 0
txt <- "0"
logic <- FALSE

#verificando a classe dos objetos
class(num)  
class(txt)
class(logic)

#Vetores

X <- c(Y, 100) #Vetor com duas dimensões. 
X

W <- X/5 # Criação de um vetor em função de outro
W

seq <- 8:16 # Criação de um vetor como uma sequeência númerica
seq

#Extração de elementos de um objeto

seq[1] #Selecionando a primeira entrada do objeto seq

seq[2:4] #Selecionando as entradas 2,3 e 4 do objeto seq


#Funções

#Função que retorna o cubo de um número
calc_X3_v1 <- function(X){
 X_3 <- X^3
 return(X_3)
}
calc_X3_v1(3)

#Funcao que retorna o cubo do número inserido e o texto indicando o número inserido e o resultado.
calc_X3_v2 <- function(X){
  X_3 <- X^3
  return(paste0("Valor: ", X, ". Valor ao cubo: ", X_3))
}
calc_X3_v2(3)
calc_X3_v2(pi)

#função calc que arredonda o resultado
calc_X3_v3 <- function(X){
  X_3 <- round(X^3, 2)
  return(paste0("Valor: ", X, ". Valor ao cubo: ", X_3)) # o número 2 no comando round é para indicar quantos decimais queremos.
}

calc_X3_v3(3)
calc_X3_v3(pi)


#Operações estatísticas.

base <- LifeCycleSavings #abrindo base auxiliar contida no pacote datasets.


mean(base$sr) #média da variável sr

sd(base$sr) #desvio-padrão da variável sr 

median(base$sr) #mediana

a <- c(mean(base$sr), sd(base$sr), median(base$sr)) #Vetor com entradas sendo funções.
a

cov(base$sr, base$pop75) # Covariância 

cor(base$sr, base$pop75) # Correlação

# Exercício 02 - Manipulando dados no R.

setwd("C:/Users/cedma/Downloads/CursoR-Itau/Introducao-ao-R") # definindo diretório de trabalho

#lendo arquivo do stata no R (.dta)
install.packages('haven')
library(haven)
data <- read_dta("monitoria1_ex1.dta") 

#Vetor com a variável de Salário

sal_mes <- data$salario_mensal
sal_mes

#Vetor com variável de Salário Anual

sal_ano <- data$salario_mensal*12
sal_ano

#Adicionando a variável sal_ano a base de dados

data <- data.frame(data, sal_ano)
head(data)
data

#criando gráfico com a desensidade estimada do Salário anaual.

plot(density(data$sal_ano),
     type = "p",
     main = "Gráfico da densidade estimada para o Salário Anual",
     xlab = "Salário Anual",
     ylab = "densidade estimada")

#Criando variável dummy de mulher

install.packages('dplyr')
library(dplyr)

data <- data %>%
  mutate(d_mulher = if_else(genero == "Feminino", 1, 0))
head(data)


#dummy para idade até 30 anos

data <- data %>%
  mutate(d_menor30 = if_else(idade <= 30, 1, 0))
head(data)

#dummy mulher com idade até 30 anos

data <- data %>%
  mutate(d_mulher_menor30 = d_mulher* d_menor30)
head(data)
data

# Conversao RG para formato numérico

#data$rg <- as.numeric(data$rg)
class(data$rg)

# Tabela de frequencia da Dummy de Mulher

table(data$d_mulher)
d_mulher

#Excluindo variável de salário mensal

names(data) #lista as variáveis do dataframe

data <- data %>%
  select(setdiff(names(data), "salario_mensal"))
head(data)

# criacao de uma base reduzida com apenas as variáveis de Nome, Sobrenome, Dummy de Mulher, Dummy de idade até 30 anos; Dummy de Mulher com idade até 30 anos e Salário Anual.

data_reduzida <- data %>%
  select(c("nome", "sobrenome", "d_mulher", "d_menor30", "d_mulher_menor30", "sal_ano"))
head(data_reduzida)

# Ccriando um objeto sendo uma base chamada "base_mulheres"

data_mulheres <- data %>%
  filter(d_mulher == 1)
head(data_mulheres)

#ordenando a base de dados por ordem alfabética

data_mulheres <- data_mulheres[order(data_mulheres$nome, data_mulheres$sobrenome),]
head(data_mulheres)

# excluindo observações da base original

data$idade

data <- data %>%
  filter(is.na(idade) != 1)

#Salvando a data_reduzida

save(data_reduzida, file = "data_reduzida.RData")
