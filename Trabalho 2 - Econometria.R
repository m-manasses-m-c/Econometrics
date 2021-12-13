library(tidyverse)
library(ggplot2)
library(forecast)
library(urca)
library(fpp)
library(tseries)
library(greybox)
library(reshape2)
library(HDeconometrics)
library(TSstudio)
library(RcppRoll)
library(lubridate)
library(clarkeTest)
#Importar os dados
IPCA_Dessazonalizado <- read_xlsx('C:\\Users\\evald\\Desktop\\IPCA_Dessazonalizado.xlsx')
dado = ts(IPCA_Dessazonalizado[,2],start = c(1998,1),frequency = 12)


# Divisão da Amostra

dado_treino <- window(dado,end=2015.99)
dado_teste <- window(dado, start=2016)

################################################################################
# Análise Preliminar                                                           #
################################################################################
autoplot(dado_treino) + ggtitle('Variação Mensal do IPCA') + ylab('%')


## É dificil afirmar a existência de estacionariedade e de sazonalidade pelo gráfico acima

# Análise de Estacionariedade

pp = ur.pp(dado_treino) # Phillips-Perron
adf = ur.df(dado_treino) # Dickey-Fuller Aumentado
summary(pp)
summary(adf)

## O teste de Phillips-Perron acusa um p-valor menor que 0.05 portanto rejeitamos a hipótese nula
## O teste de ADF também rejeita a hipótese nula
## Assumimos estacionariedade da série,não sendo necessária a diferenciação

# Análise de Sazonalidade

ggseasonplot(dado) + ggtitle("Gráfico Sazonal: Variação Mensal do IPCA") +
  ylab('%')



ggsubseriesplot(dado) + ggtitle("Gráfico Sazonal: Variação Mensal do IPCA") +
  ylab('%')



################################################################################
#                               RESULTADOS

# O teste de Phillips-Perron acusa um p-valor menor que 0.05 portanto rejeitamos
# a hipótese nula;

# O teste de ADF também rejeita a hipótese nula;

# Assumimos estacionariedade da série,não sendo necessária a diferenciação;

# Uma análise do gráfico ggseasonplot acusa uma possibilidade de sazonalidade nos
# meses de julho e março;

# No segundo gráfico, das subséries, é observável que média das variações em todos
# os meses apresentam uma diferença negligenciável mas com detalhe para ao aparente
# descolamento do mês de julho frente à média geral.
################################################################################

################################################################################
# Parametrização do Modelo                                                     #
################################################################################

# ACF e PACF para observar possível sazonalidade e observar os parâmetros ótimos para estimação de um modelo
acf(dado_treino,21)
acf(dado_treino,length(dado_treino))
pacf(dado_treino,21)
pacf(dado_treino,length(dado_treino))

################################################################################
#                               RESULTADOS
#
# As funções de autocorrelação e autocorrelação parcial acusam a possibilidade 
# de parâmetros (q,p) igual a (1,0). Entretanto, foi solicitado para o trabalho 
# que utilizássemos um modelo AR(q), portanto apenas o parâmetro q será utilizado.
# Ademais, o gráfico também apresenta possível signficância no lag 13, portanto 
# também será testado o modelo AR(13).
################################################################################

################################################################################
#                         Simulação dos Modelos AR                             #
################################################################################

## AR(1)
ar1 <- arima(dado_treino, order = c(1,0,0))
print(summary(ar1))
checkresiduals(ar1)
ar1_fit <- dado_treino - residuals(ar1)

## AR(13)
ar2 <- arima(dado_treino, order = c(13,0,0))
print(summary(ar2))
checkresiduals(ar2)
ar2_fit <- dado_treino - residuals(ar2)
#Gráficos
z <- cbind(as_tibble(IPCA_Dessazonalizado$Data[1:length(dado_treino)]),dado_treino,ar1_fit,ar2_fit)
colnames(z) <- c('T','Yt','AR(1)','AR(13)')
z <- melt(z,id.vars = 'T', variable.name = 'Série')
ggplot(z,aes(T,value)) + geom_line() + facet_grid(Série ~ .)

###Comparativo de Critérios de Informação
AIC_1 <- AIC(ar1)
BIC_1 <- BIC(ar1)

AIC_2 <- AIC(ar2)
BIC_2 <- BIC(ar2)

crit <- as_tibble(cbind(AIC_1,AIC_2,BIC_1,BIC_2))
view(crit)

Box.test(resid(ar1),type = 'Ljung-Box') #Resíduos São Ruido Branco


################################################################################
#                                 RESULTADOS
#
# Uma vez simuladas as séries de acordo com a amostra de treino podemos observar
# que os resíduos de ambos os modelos aparentam seguir uma distribuição normal. 
# Além disso, vemos que o resíduo do modelo AR(1) apresenta autocorrelação levemente 
# significativa em alguns lags, enquanto o modelo AR(13) não.
#
# O modelo AR(1) apresentou BIC 40 pontos menor do que o modelo AR(13), portanto,
# ainda que o modelo AR(13) tenha apresentado AIC um pouco menor, será escolhido o modelo
################################################################################

################################################################################
#                               Previsão
################################################################################

dummy <- ts(0*1:length(dado),start = 1998,end=2022,frequency=12)
for (i in 1:length(dado_treino)){
  dummy[i] = dado_treino[i]
}
windowsize = 50
for (i in length(dado_treino):length(dado)){
  arr = arima(dado[(i-windowsize):i],order = c(1,0,0))
  dummy[i] = as.numeric(forecast(arr,h=1)$pred)
}
rw <- ts(0*1:length(dado_teste),start = 2016,frequency=12)
for(i in 1:length(dado)){  
  x <- window(dado, end=(2015.99 + (i-1)/12))
  rw[i] <- tail(x,1)
}
prev <- window(dummy,start = 2016,end=2021.79)
z <- cbind(dado_teste,prev,rw)
na.omit(z)
colnames(z) <- c('Amostra de Teste','Rolling Window (50)','Random-Walk')
autoplot(z)


# Teste Diebold-Mariano-West

dm.test(rw,prev,'greater')
dm.test(rw,prev,'two.sided')
dm.test(rw,prev,'less')

#Teste Clark e West ( A FAZER )!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  
################################################################################
#                             EXERCÍCIOS 2,3,4                                 #
################################################################################

##########################################################################
# GERANDO A SÉRIE (FINALIZADO)
##########################################################################

seq <- 1:100
# Definindo resultado aleatório a ser obtido

set.seed(123)

# Gerando ruído branco

e <- rt(100,5)

# Processo Yt = u + c1*Yt-1 + e, onde Y1 = e1

## Definindo as constantes
mu <- 0.5
c2 <- 0.7

## Criando o objeto da série e estimando seus primeiros valores
y <- list()
y <- unlist(y)
y[1] <- mu + e[1]
y[2] <- mu + c1*y[1]+e[2]

## Gerando os demais valores da série
for (i in 2:100){
  y[i] <- mu + c1*y[i-1] +e[i]
}
ts.plot(y)

#################################################################################
# PREVISÃO 
###############################################################################
# Objetivos:
# Precisamos prever usando random walk e MQO(lm(yt+1 ~ y_t)) e testar



y_t1 = na.omit((lag(as.numeric(y),1)))
y_t2 = na.omit(y[2:100])
lm <-  lm(y_t2~y_t1)
y_t3 <- rnorm(100,0,sd=0)
for ( i in 1:length(y_t1)){
y_t3[i] = 0.6335 + (0.4308*y_t1[i])
}
lm$coefficients[2]

for (i in 51:100){
  y_t1 = lag(as.numeric(y[i-50:i]))
  y_t2 = y[(i-49):(99+i-50)]
  lm <- lm(y_t2 ~ y_t1)
  y_t3[i] =  lm$coefficients[1] + (lm$coefficients[2]*y_t1[i])
}
