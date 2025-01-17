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


# Divis�o da Amostra

dado_treino <- window(dado,end=2015.99)
dado_teste <- window(dado, start=2016)

################################################################################
# An�lise Preliminar                                                           #
################################################################################
autoplot(dado_treino) + ggtitle('Varia��o Mensal do IPCA') + ylab('%')


## � dificil afirmar a exist�ncia de estacionariedade e de sazonalidade pelo gr�fico acima

# An�lise de Estacionariedade

pp = ur.pp(dado_treino) # Phillips-Perron
adf = ur.df(dado_treino) # Dickey-Fuller Aumentado
summary(pp)
summary(adf)

## O teste de Phillips-Perron acusa um p-valor menor que 0.05 portanto rejeitamos a hip�tese nula
## O teste de ADF tamb�m rejeita a hip�tese nula
## Assumimos estacionariedade da s�rie,n�o sendo necess�ria a diferencia��o

# An�lise de Sazonalidade

ggseasonplot(dado) + ggtitle("Gr�fico Sazonal: Varia��o Mensal do IPCA") +
  ylab('%')



ggsubseriesplot(dado) + ggtitle("Gr�fico Sazonal: Varia��o Mensal do IPCA") +
  ylab('%')



################################################################################
#                               RESULTADOS

# O teste de Phillips-Perron acusa um p-valor menor que 0.05 portanto rejeitamos
# a hip�tese nula;

# O teste de ADF tamb�m rejeita a hip�tese nula;

# Assumimos estacionariedade da s�rie,n�o sendo necess�ria a diferencia��o;

# Uma an�lise do gr�fico ggseasonplot acusa uma possibilidade de sazonalidade nos
# meses de julho e mar�o;

# No segundo gr�fico, das subs�ries, � observ�vel que m�dia das varia��es em todos
# os meses apresentam uma diferen�a negligenci�vel mas com detalhe para ao aparente
# descolamento do m�s de julho frente � m�dia geral.
################################################################################

################################################################################
# Parametriza��o do Modelo                                                     #
################################################################################

# ACF e PACF para observar poss�vel sazonalidade e observar os par�metros �timos para estima��o de um modelo
acf(dado_treino,21)
acf(dado_treino,length(dado_treino))
pacf(dado_treino,21)
pacf(dado_treino,length(dado_treino))

################################################################################
#                               RESULTADOS
#
# As fun��es de autocorrela��o e autocorrela��o parcial acusam a possibilidade 
# de par�metros (q,p) igual a (1,0). Entretanto, foi solicitado para o trabalho 
# que utiliz�ssemos um modelo AR(q), portanto apenas o par�metro q ser� utilizado.
# Ademais, o gr�fico tamb�m apresenta poss�vel signfic�ncia no lag 13, portanto 
# tamb�m ser� testado o modelo AR(13).
################################################################################

################################################################################
#                         Simula��o dos Modelos AR                             #
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
#Gr�ficos
z <- cbind(as_tibble(IPCA_Dessazonalizado$Data[1:length(dado_treino)]),dado_treino,ar1_fit,ar2_fit)
colnames(z) <- c('T','Yt','AR(1)','AR(13)')
z <- melt(z,id.vars = 'T', variable.name = 'S�rie')
ggplot(z,aes(T,value)) + geom_line() + facet_grid(S�rie ~ .)

###Comparativo de Crit�rios de Informa��o
AIC_1 <- AIC(ar1)
BIC_1 <- BIC(ar1)

AIC_2 <- AIC(ar2)
BIC_2 <- BIC(ar2)

crit <- as_tibble(cbind(AIC_1,AIC_2,BIC_1,BIC_2))
view(crit)

Box.test(resid(ar1),type = 'Ljung-Box') #Res�duos S�o Ruido Branco


################################################################################
#                                 RESULTADOS
#
# Uma vez simuladas as s�ries de acordo com a amostra de treino podemos observar
# que os res�duos de ambos os modelos aparentam seguir uma distribui��o normal. 
# Al�m disso, vemos que o res�duo do modelo AR(1) apresenta autocorrela��o levemente 
# significativa em alguns lags, enquanto o modelo AR(13) n�o.
#
# O modelo AR(1) apresentou BIC 40 pontos menor do que o modelo AR(13), portanto,
# ainda que o modelo AR(13) tenha apresentado AIC um pouco menor, ser� escolhido o modelo
################################################################################

################################################################################
#                               Previs�o
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
#                             EXERC�CIOS 2,3,4                                 #
################################################################################

##########################################################################
# GERANDO A S�RIE (FINALIZADO)
##########################################################################

seq <- 1:100
# Definindo resultado aleat�rio a ser obtido

set.seed(123)

# Gerando ru�do branco

e <- rt(100,5)

# Processo Yt = u + c1*Yt-1 + e, onde Y1 = e1

## Definindo as constantes
mu <- 0.5
c2 <- 0.7

## Criando o objeto da s�rie e estimando seus primeiros valores
y <- list()
y <- unlist(y)
y[1] <- mu + e[1]
y[2] <- mu + c1*y[1]+e[2]

## Gerando os demais valores da s�rie
for (i in 2:100){
  y[i] <- mu + c1*y[i-1] +e[i]
}
ts.plot(y)

#################################################################################
# PREVIS�O 
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
