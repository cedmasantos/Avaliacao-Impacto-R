# Curso Avançado de Avaliação de Políticas Públicas e Projetos Sociais - Itaú Social

#Aula 02 - MQO, Mínimos Quadrados Ordinários.

#Exercício 01

setwd("C:/Users/cedma/Downloads/CursoR-Itau/Aula 2 - Causalidade e MQO") #definindo diretório de tabalho

library(haven) #pacote para abrir base de dados no formato .dta

data1 <- read_dta("BWGHT.dta")

# A base BWGHT.dta é utilizada para estudar determinantes do peso ao nascer (birth weight). Fonte: Introductory Econometrics: A Modern Approach do Jeffrey Wooldridge

# Dicionário de dados:

#bwght: peso do bebê ao nascer (em onças).
#cigs: número médio de cigarros fumados por dia durante a gravidez.
#faminc: renda familiar em milhares de dólares.
#motheduc: anos de escolaridade da mãe.
#fatheduc: anos de escolaridade do pai.
#parity: número de filhos anteriores da mãe.
#male: indicador se o bebê é do sexo masculino (1 = sim, 0 = não).

# a) Estime modelo e sumaraze os resultados em tabela.

mqo_1 <- lm(bwght ~ cigs + parity + motheduc + fatheduc, data = data)

# Visualizando os resultados: summary

summary(mqo_1)

# stargazer: pacote que permite a visualização da saída da regressão em uma tabela limpa e organizada.
#install.packages('stargazer')
library(stargazer)

#Visualizando os resultados pelo stargazer

stargazer(mqo_1,
          type="text",
          title = "Regressão linar de peso ao nascer",
          dep.var.labels = "peso ao nascer",
          stlyer = "aer",
          model.names=TRUE)


#Interpretação:

# A equação estimada pelo MQO é:
#   
# bwght =113.736−0.606⋅cigs+1.765⋅parity−0.276⋅motheduc+0.580⋅fatheduc
# 
# Intercepto (113.736): Quando todas as variáveis explicativas são zero, o peso previsto ao nascer seria 113.736 onças. Esse valor não tem um significado direto, pois dificilmente ocorre na prática.
# 
# Cigs (-0.606): Cada cigarro adicional fumado por dia durante a gravidez reduz, em média, o peso ao nascer em 0.606 onças. O coeficiente é estatisticamente significativo (
#   p<0.01), ou seja, há forte evidência de que o tabagismo da mãe impacta negativamente o peso do bebê.
# 
# Parity (+1.765): Cada filho anterior da mãe aumenta, em média, o peso ao nascer em 1.765 onças. O coeficiente é significativo ao nível de 
# 1%, sugerindo que bebês de mães com mais filhos anteriores tendem a nascer com um peso um pouco maior.
# 
# Motheduc (-0.276): Cada ano adicional de escolaridade da mãe reduz, em média, o peso ao nascer em 0.276 onças, mas esse efeito não é estatisticamente significativo (
# p>0.1). Ou seja, não podemos concluir que a educação da mãe tem um impacto claro no peso do bebê com base nesses dados.
# 
# Fatheduc (+0.580): Cada ano adicional de escolaridade do pai aumenta, em média, o peso ao nascer em 0.580 onças. Esse efeito é significativo ao nível de 
# 5%, sugerindo que a educação paterna pode estar associada a um maior peso ao nascer.



# b) Verifique se após controlarmos pela quantidade de cigarros consumidos por dia pela mãe
#durante a gravidez, ordem de nascimento e renda familiar, a educação da mãe e do pai,
#conjuntamente, não têm efeito sobre o peso de nascimento.

# car: pacote para realizar o teste de significância conjunta

#install.packages('car')
library (car)

#Testando conjuntamente as hipóteses de que o coeficiente de educação da mãe e do pai são iguais a zero:

linearHypothesis(mqo_1, c("motheduc=0", "fatheduc=0"))

#C) Reestime o modelo, ainda por MQO, mas agora considerando a variável dependente em logaritmo natural.

mqo_2 <- lm(lbwght ~ cigs + parity + lfaminc + motheduc + fatheduc, data = data)
 
# Sumarize os dois modelos em uma só tabela e compare os coeficientes estimados por MQO.

stargazer(mqo_1, mqo_2,
          type="text",
          title = "Regressão linear de peso ao nascer",
          dep.var.labels = c("peso ao nascer", "log(peso ao nascer)"),
          stlyer="aer",
          model.names = TRUE)
