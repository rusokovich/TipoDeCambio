#Limpiar el area de trabajo 
rm(list=ls())


# librerias necesarias 

library(dplyr)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(zoo)
library(TSA)
library(forecast)
library(data.table)
library(tseries)
library(fGarch)
library(rugarch)


# 2015-1-15 Inicio de nueva politica cambiaria


# determinar el area de trabajo 
setwd('/Users/fkkarpuk/Desktop/Projects/PresentacionTipoCambio')



# subir los datos, 

antiguo.pronostico<-rio::import('PronosticoAntiguo.csv')
head(antiguo.pronostico)
datos.tipo.cambio<-rio::import('DatosTipoCambio.csv')
head(datos.tipo.cambio)
#datos.tipo.cambio$DateDt<-as.Date(datos.tipo.cambio$Datedt)
# exploracion


str(antiguo.pronostico)
str(datos.tipo.cambio)


## Agregar paquetes adicionales de analisis de series de tiempo
setwd('/Users/fkkarpuk/Desktop/CursoUCC/ModelosEstadisticos/SeriesDeTiempo')

source("summary.arima.R")
source("TS.diag.R")



## Creacion de series de tiempo

ts.antiguo.pronostico.compra<-ts(antiguo.pronostico$FCExchangeRateBidCR, ,start = c(2024,01), frequency =365)




ts.datos.tipo.cambio.compra.2024<-ts(datos.tipo.cambio[year(datos.tipo.cambio$DateDt)==2024, "Compra"]
                                ,start = c(2024,01), frequency =365)

## Comparacion de resultados desde el ultimo entrenamiento en setiembre 

plot(ts.datos.tipo.cambio.compra.2024,ylim=c(490,540),xlab="periodo", ylab="tipo cambio", main="Antiguo pronostico entrenado Setiembre")
lines(ts.antiguo.pronostico.compra, col="red")

mean(abs(ts.antiguo.pronostico.compra[1:length(ts.datos.tipo.cambio.compra.2024)]/ts.datos.tipo.cambio.compra.2024-1))*100

####### Analisis de transformaciones ############

### Analisis del nuevo metodo de entrenamiento 
ts.datos.tipo.cambio.compra<-ts(datos.tipo.cambio$Compra)


### Diferenciacion recomendada 


forecast::ndiffs(ts.datos.tipo.cambio.compra)


# aca logra la estacionariedad 

# H0: no hay estacionariedad
# H1: hay estacionariedad
tseries::adf.test(ts.datos.tipo.cambio.compra)

# Se recomienda una diferenciacion 

plot(diff(ts.datos.tipo.cambio.compra))

summary(diff(ts.datos.tipo.cambio.compra))
## Posibles transformaciones de la curva 

MASS::boxcox(ts.datos.tipo.cambio.compra~time(ts.datos.tipo.cambio.compra)
             , lambda = seq(-10,10,0.001))

lambda<-round(forecast::BoxCox.lambda(x=ts.datos.tipo.cambio.compra
                                       , lower = 0
                                       , upper = 1
)
)

# parece que el lamda es 0 por lo que se podria considerar una transformacion logaritmica 

plot(log(ts.datos.tipo.cambio.compra))


###### Analsis de transformaciones ############


#### Componente MA


# Analisis de la funcion ACF (valores MA) freq 7
acf(c(diff(ts.datos.tipo.cambio.compra)),lag.max = 7, main="Dependencia lineal de una semana")

q.7<-6
# Valor recomendado 6 

# Analisis de la funcion ACF (valores MA) freq 30
acf(c(diff(ts.datos.tipo.cambio.compra)),lag.max = 30, main="Dependencia lineal de un mes")

q.30<-28
# Valor recomendado 28


# construccion de los valores 
correlograma.completo<-acf(c(diff(ts.datos.tipo.cambio.compra)),lag.max = 365, plot = FALSE)


# Calculo de phi 

# calculo de los limites de el nivel de significancia es decir las barras
alpha<-0.95
ciz<-c(-1,1)*(-qnorm((1-alpha)/2)/sqrt(correlograma.completo$n.used-3))
cir <- (exp(2*ciz)-1)/(exp(2*ciz)+1)  
# rho <- (exp(2 * 1.645 / sqrt(correlograma.completo$n.used - 3)) - 1) / (exp(2 * 1.645 / sqrt(correlograma.completo$n.used - 3)) + 1)

# verificacion visual del coeficiente 
# acf(diff(ts.datos.tipo.cambio.compra),lag.max = 6)




### Construccion de la tabla de valores significativos

valores.correlograma <- as.data.frame(correlograma.completo$acf)
valores.correlograma$Lag <- as.numeric(row.names(valores.correlograma))-1
colnames(valores.correlograma)<-c("valores","lag")
valores.correlograma$valores.absolutos<-abs(valores.correlograma$valores)
# valores.correlograma$ranking.valores<-dense_rank(desc(valores.correlograma$valores.absolutos))
valores.correlograma$multiplo30 <- valores.correlograma$lag %% 30 == 0 
valores.correlograma$multiplo7 <- valores.correlograma$lag %% 7 == 0 
valores.correlograma$valorsignificativo <- ifelse(valores.correlograma$valores.absolutos > abs(ciz[1]), 1, 0)

#str(valores.correlograma)
## Analisis de elementos SMA 

freq7 <- valores.correlograma %>%
  filter(multiplo7 == TRUE) %>%  
  select(-c(multiplo30)) %>%  
  arrange(lag)        


Q.7<-12
## Pareceria que un valor bueno para freq 7 seria 12 para el SMA 

freq30 <- valores.correlograma %>%
  filter(multiplo30 == TRUE) %>%  # Filter rows where multiplo30 is TRUE
  select(-c(multiplo7)) %>% 
  arrange(lag)        

## Pareceria que un valor bueno para freq 30 seria 4 para el SMA 
Q.30<-4

#### Componente AR


#### Calculo del valor AR con la funcion pacf 

# Analisis de la funcion PACF (valores AR) freq 7
pacf(c(diff(ts.datos.tipo.cambio.compra)),lag.max = 6)

p.7<-6

# El valor 6 parece significativo 
# Analisis de la funcion ACF (valores AR) freq 30
pacf(c(diff(ts.datos.tipo.cambio.compra)),lag.max = 29)
# El valor 28 parece significativo 

p.30<-28


# Calculo de phi 


correlograma.parcial<-pacf(c(diff(ts.datos.tipo.cambio.compra)),lag.max = 365, plot = FALSE)

# rho <- (exp(2 * 1.645 / sqrt(correlograma.completo$n.used - 3)) - 1) / (exp(2 * 1.645 / sqrt(correlograma.completo$n.used - 3)) + 1)


## Verificacion grafica
pacf(c(diff(ts.datos.tipo.cambio.compra)),lag.max = 6, ylim=c(0.03,0.04))


### Construccion de la tabla de valores significativos

valores.correlograma.parcial <- as.data.frame(correlograma.parcial$acf)
valores.correlograma.parcial$Lag <- row.names(valores.correlograma.parcial)
colnames(valores.correlograma.parcial)<-c("valores","lag")
valores.correlograma.parcial$lag <- as.numeric(valores.correlograma.parcial$lag)
valores.correlograma.parcial$valores.absolutos<-abs(valores.correlograma.parcial$valores)
# valores.correlograma$ranking.valores<-dense_rank(desc(valores.correlograma$valores.absolutos))
valores.correlograma.parcial$multiplo30 <- valores.correlograma.parcial$lag %% 30 == 0 
valores.correlograma.parcial$multiplo7 <- valores.correlograma.parcial$lag %% 7 == 0 
valores.correlograma.parcial$valorsignificativo <- ifelse(valores.correlograma.parcial$valores.absolutos > abs(ciz[1]), 1, 0)


## Analisis de elementos SRA

freq7 <- valores.correlograma.parcial %>%
  filter(multiplo7 == TRUE) %>%  
  select(-c(multiplo30)) %>%  
  arrange(lag)   

P.7<-13

## Pareceria que un valor bueno para freq 7 seria 13 para el SMA 


freq30 <- valores.correlograma.parcial %>%
  filter(multiplo30 == TRUE) %>%  # Filter rows where multiplo30 is TRUE
  select(-c(multiplo7)) %>% 
  arrange(lag)    

P.30<-0

## Pareceria que un valor bueno para freq 30 seria 0 para el SAR


#### Entrenamiento de los modelos 



# Segmentacion del conjunto de entrenamiento y prueba 


dia.corte<-as.Date('2024-09-01')
datos.ts.entrenamiento<-datos.tipo.cambio[datos.tipo.cambio$DateDt <= dia.corte,]
datos.ts.prueba<-datos.tipo.cambio[datos.tipo.cambio$DateDt > dia.corte,]


## Histogramas 

hist(datos.ts.entrenamiento$Compra,xlab="Tipo de Cambio"
     ,ylab="Frecuencia", main = "",breaks=50, xaxt = "n" )

axis(1, at = seq(min(datos.ts.entrenamiento$Compra)
                 , max(datos.ts.entrenamiento$Compra),by = 5 ))

## Cortes importantes 
n.entrenamiento <- length(datos.ts.entrenamiento$Compra)

indices.entrenamiento <- 1:n.entrenamiento
indices.test <- (n.entrenamiento + 1):(n.entrenamiento + length(datos.ts.prueba$Compra))

## Tiempo de Prediccion

intervalos.pred<-max(indices.test)-min(indices.test)


### Fechas de Prediccion 

fecha.prediccion<-dia.corte+intervalos.pred

# numero de dias que se have backtesting 
revision.backtesting <- 150
fecha.backtesting<-dia.corte-revision.backtesting




plot(indices.entrenamiento, datos.ts.entrenamiento$Compra, type = "l"
     ,xlab = "Tiempo"
     , ylab = "Compra"
     ,xlim=c(3000,3600)
     ,ylim=c(490,580)
     # , xlim = c(1, training_length + length(datos.ts.prueba$Compra)),
     ,main = "Visualzacion de los datos")

lines(indices.test, datos.ts.prueba$Compra, col = "red")


####### Modelos ARIMA ################

######## Frecuencia de 7 

X <- ts(datos.ts.entrenamiento$Compra)

#ts.real<-ts(datos.ts.prueba$Compra,frequency = 7)
  

fixed <- c(NA,0,NA,NA,NA,NA # Coeficientes AR 
           ,NA,0,NA,NA,NA,0 # Coeficientes MA
          )

#### Sin Ajuste
# modelo.arima.freq7<- forecast::Arima(X, order = c(p.7,1,q.7)
#                                      #, seasonal = c(2,0,4)
#                                      , lambda = 0
#                                      #, fixed = fixed
# )

tiempo.ejecucion.inicio<-Sys.time()

modelo.arima.freq7<- forecast::Arima(X, order = c(p.7,1,q.7)
                                     #, seasonal = c(2,0,4)
                                     , lambda = 0
                                     , fixed = fixed
)

tiempo.ejecucion.final<-Sys.time()


# Revision del AIC 
summary(modelo.arima.freq7)

aic.modelo.arima.freq7<- round(summary(modelo.arima.freq7)$aic)

print(paste0("El AIC del modelo es de: ", aic.modelo.arima.freq7))

#"El AIC del modelo es de: -32349"

# Revision de raices inversas 
plot(modelo.arima.freq7)

# fixed <- rep(NA, length(modelo.arima.freq7$coef))

# Revision de siginificancia de los coeficientes
summary_arima(modelo.arima.freq7, fixed = fixed)


# Prueba de normalidad de los residuos 

# H0: residuos normales 
# H1: residuos no normales

ks.test(scale(c(modelo.arima.freq7$residuals)), "pnorm")

# p-value = 2.2e-16 se rechaza H0 


# Homocedasticidad 

# H0: residuos Homcedasticos 
# H1: residuos no homocedasticos 
lmtest::bptest(lm(modelo.arima.freq7$residuals~ time(modelo.arima.freq7$residuals)))


# p-value = 2.2e-16 se rechaza H0 

# Revisar valores Ljung 
#TS.diag(c(modelo.arima.freq7$residuals),lag= 40)


LSTS::ts.diag(c(modelo.arima.freq7$res), lag = 50)[[3]]

# No pasa blancura 

# Visualizacion de los datos 
par(mfrow=c(1,1))

#### Pronosticos 

pred.modelo.arima.freq7<- forecast::forecast(modelo.arima.freq7, h = intervalos.pred+1)





# Revision Visual de ajuste de los residuos 
plot(X)
lines(pred.modelo.arima.freq7$fitted, col = "red")

plot(X
     , xlim = c(max(indices.entrenamiento)-revision.backtesting,max(indices.entrenamiento))
     , ylim = c(495,530)
     , xlab="Periodo de Tiempo en días"
     , ylab = " Tipo de Cambio"
     , main = paste("Prueba residuos fitted desde ", fecha.backtesting , " hasta ", dia.corte)
     )
lines(pred.modelo.arima.freq7$fitted, col = "red")



# Revision visual de la Prediccion 

margen.dias<-50


plot(pred.modelo.arima.freq7
     ,xlim=c(min(indices.test)-margen.dias,max(indices.test))
     ,ylim=c(490,580)
     , xlab="Periodo de Tiempo en días"
     , ylab = "Tipo de Cambio"
     , main = paste("Prueba backtesting pronóstico desde ", dia.corte , " hasta ", fecha.prediccion)
     
)
lines(indices.test, datos.ts.prueba$Compra, col = "red")


### Calculo MAPE 

mape.modelo.arima.freq7<- mean(abs(pred.modelo.arima.freq7$mean/datos.ts.prueba$Compra-1))*100

paste("El Mape del modelo es: ", round(mape.modelo.arima.freq7,2), "%")

paste("El tiempo de ejecucion del modelo es de ", round(tiempo.ejecucion.final-tiempo.ejecucion.inicio)," segundos")





######## Frecuencia de 30

X <- ts(datos.ts.entrenamiento$Compra)

#ts.real<-ts(datos.ts.prueba$Compra,frequency = 7)

# para  Junio 

# fixed <- c(
#   # Coeficientes AR
# NA,0,NA,NA,0,NA,NA,NA,0,0,NA,0,0,NA,NA,NA,NA,0,NA,NA,NA,NA,0,0,NA,0,0,NA, 
# # Coeficientes MA
# NA,NA,NA,NA,NA,NA,NA,NA,0,0,NA,0,0,NA,NA,NA,NA,0,NA,NA,NA,NA,NA,0,NA,0,0,NA
# )

# Para Setiembre
fixed <- c(
  # Coeficientes AR
  NA,NA,NA,NA,NA,NA,NA,NA,0,0,NA,0,0,NA,NA,NA,NA,0,NA,NA,NA,NA,0,0,NA,0,0,NA, 
  # Coeficientes MA
  NA,NA,NA,NA,NA,NA,NA,NA,0,0,NA,0,0,NA,NA,NA,NA,0,NA,NA,NA,NA,NA,0,NA,0,0,NA
)


#### Sin Ajuste
# modelo.arima.freq30<- forecast::Arima(X, order = c(p.30,1,q.30)
#                                      #, seasonal = c(2,0,4)
#                                      , lambda = 0
#                                      #, fixed = fixed
# )
# 
# fixed <- rep(NA, length(modelo.arima.freq30$coef))


#### Creacion del modelo 

tiempo.ejecucion.inicio<-Sys.time()

modelo.arima.freq30<- forecast::Arima(X, order = c(p.30,1,q.30)
                                     #, seasonal = c(2,0,4)
                                     , lambda = 0
                                     , fixed = fixed
)

tiempo.ejecucion.final<-Sys.time()


# Revision del AIC 
summary(modelo.arima.freq30)

aic.modelo.arima.freq30<- round(summary(modelo.arima.freq30)$aic)


print(paste0("El AIC del modelo es de: ", round(summary(modelo.arima.freq30)$aic)))

#"El AIC del modelo es de: -32463" sin ajuste 

#"El AIC del modelo es de: -32490" con ajuste 


# Revision de raices inversas 
plot(modelo.arima.freq30)


# Revision de siginificancia de los coeficientes
summary_arima(modelo.arima.freq30, fixed = fixed)


# Prueba de normalidad de los residuos 

# H0: residuos normales 
# H1: residuos no normales

ks.test(scale(c(modelo.arima.freq30$residuals)), "pnorm")

# p-value = 2.2e-16 se rechaza H0 


# Homocedasticidad 

# H0: residuos Homcedasticos 
# H1: residuos no homocedasticos 
lmtest::bptest(lm(modelo.arima.freq30$residuals~ time(modelo.arima.freq30$residuals)))


# p-value = 2.2e-16 se rechaza H0 

# Revisar valores Ljung 
#TS.diag(c(modelo.arima.freq7$residuals),lag= 40)


LSTS::ts.diag(c(modelo.arima.freq30$res), lag = 90)[[3]]



# Pasa el test de Blancura 

# Visualizacion de los datos 
par(mfrow=c(1,1))

#### Pronosticos 

pred.modelo.arima.freq30<- forecast::forecast(modelo.arima.freq30, h = intervalos.pred+1)





# Revision Visual de ajuste de los residuos 
plot(X)
lines(pred.modelo.arima.freq30$fitted, col = "red")

plot(X
     , xlim = c(max(indices.entrenamiento)-revision.backtesting,max(indices.entrenamiento))
     , ylim = c(495,530)
     , xlab="Periodo de Tiempo en días"
     , ylab = " Tipo de Cambio"
     , main = paste("Prueba residuos fitted desde ", fecha.backtesting , " hasta ", dia.corte)
)
lines(pred.modelo.arima.freq30$fitted, col = "red")



# Revision visual de la Prediccion 

margen.dias<-50


plot(pred.modelo.arima.freq30
     ,xlim=c(min(indices.test)-margen.dias,max(indices.test))
     ,ylim=c(490,580)
     , xlab="Periodo de Tiempo en días"
     , ylab = "Tipo de Cambio"
     , main = paste("Prueba backtesting pronóstico desde ", dia.corte , " hasta ", fecha.prediccion)
     
)
lines(indices.test, datos.ts.prueba$Compra, col = "red")


### Calculo MAPE 

mape.modelo.arima.freq30<- mean(abs(pred.modelo.arima.freq30$mean/datos.ts.prueba$Compra-1))*100

paste("El Mape del modelo es: ", round(mape.modelo.arima.freq30,2), "%")
#"El Mape del modelo es:  1.37 %"
paste("El tiempo de ejecucion del modelo es de ", round(tiempo.ejecucion.final-tiempo.ejecucion.inicio)," segundos")

"El tiempo de ejecucion del modelo es de  2  segundos"



###### MODELOS SARIMA ###############

######## Frecuencia de 7 

X <- ts(datos.ts.entrenamiento$Compra, frequency = 7)


#ts.real<-ts(datos.ts.prueba$Compra,frequency = 7,start = c(18, 4) )

## Para Pronostico de Junio
# fixed <- c(
#   
# NA,NA,NA,0,NA,NA, # Coeficientes AR
# NA,NA,NA,0,NA,NA, # Coeficientes MA
# NA,0,NA,NA,NA,NA,NA,NA,NA,0,NA,0,0, # Coeficientes SAR
# NA,NA,NA,NA,NA,NA,NA,NA,NA,0,NA,0  # Coeficientes SMA
# )


## Para Pronostico de Setiembre
fixed <- c(
  
  NA,NA,NA,NA,NA,NA, # Coeficientes AR
  NA,NA,NA,NA,NA,NA, # Coeficientes MA
  NA,0,NA,NA,NA,NA,NA,NA,NA,0,NA,0,0, # Coeficientes SAR
  NA,NA,NA,NA,NA,NA,NA,NA,NA,0,NA,0  # Coeficientes SMA
)


#### Sin Ajuste

# modelo.sarima.freq7<- forecast::Arima(X, order = c(p.7,1,q.7)
#                                      , seasonal = c(P.7,0,Q.7)
#                                      #,include.drift = T
#                                      , lambda = 0
#                                      #, fixed = fixed
# )
# 
# fixed <- rep(NA, length(modelo.sarima.freq7$coef))

tiempo.ejecucion.inicio<-Sys.time()

modelo.sarima.freq7<- forecast::Arima(X, order = c(p.7,1,q.7)
                                     , seasonal = c(P.7,0,Q.7)
                                     , lambda = 0
                                     , fixed = fixed
)

tiempo.ejecucion.final<-Sys.time()


# Revision del AIC 
summary(modelo.sarima.freq7)

aic.modelo.sarima.freq7<- round(summary(modelo.sarima.freq7)$aic)

print(paste0("El AIC del modelo es de: ", aic.modelo.sarima.freq7))

#"El AIC del modelo es de: -32553" sin ajuste

# "El AIC del modelo es de: -32539" con ajuste


# Revision de raices inversas 
plot(modelo.sarima.freq7)



# Revision de siginificancia de los coeficientes
summary_arima(modelo.sarima.freq7, fixed = fixed)


# Prueba de normalidad de los residuos 

# H0: residuos normales 
# H1: residuos no normales

ks.test(scale(c(modelo.sarima.freq7$residuals)), "pnorm")

# p-value = 2.2e-16 se rechaza H0 


# Homocedasticidad 

# H0: residuos Homcedasticos 
# H1: residuos no homocedasticos 
lmtest::bptest(lm(modelo.sarima.freq7$residuals~ time(modelo.sarima.freq7$residuals)))


# p-value = 2.2e-16 se rechaza H0 

# Revisar valores Ljung 
#TS.diag(c(modelo.arima.freq7$residuals),lag= 40)


LSTS::ts.diag(c(modelo.sarima.freq7$res), lag = 90)[[3]]

# Pasa el test blancura 

# Visualizacion de los datos 
par(mfrow=c(1,1))

#### Pronosticos 


pred.modelo.sarima.freq7<- forecast::forecast(modelo.sarima.freq7, h = intervalos.pred+1)


# Revision Visual de ajuste de los residuos 
plot(X)
lines(pred.modelo.sarima.freq7$fitted, col = "red")

plot(X
     , xlim = c((max(indices.entrenamiento)-revision.backtesting)/7,(max(indices.entrenamiento))/7)
     , ylim = c(495,530)
     , xlab="Periodo de Tiempo en días"
     , ylab = " Tipo de Cambio"
     , main = paste("Prueba residuos fitted desde ", fecha.backtesting , " hasta ", dia.corte)
)
lines(pred.modelo.sarima.freq7$fitted, col = "red")



# Revision visual de la Prediccion 

margen.dias<-50/7


plot(pred.modelo.sarima.freq7
     ,xlim=c((min(indices.test)-margen.dias)/7,(max(indices.test))/7)
     ,ylim=c(490,580)
     , xlab="Periodo de Tiempo en semanas"
     , ylab = "Tipo de Cambio"
     , main = paste("Prueba backtesting pronóstico desde ", dia.corte , " hasta ", fecha.prediccion)
     
)
#lines(indices.test/7, datos.ts.prueba$Compra, col = "red")
lines(as.numeric(time(pred.modelo.sarima.freq7$mean)), datos.ts.prueba$Compra, col = "red")

### Calculo MAPE 

mape.modelo.sarima.freq7<- mean(abs(pred.modelo.sarima.freq7$mean/datos.ts.prueba$Compra-1))*100

paste("El Mape del modelo es: ", round(mape.modelo.sarima.freq7,2), "%")

paste("El tiempo de ejecucion del modelo es de ", round(tiempo.ejecucion.final-tiempo.ejecucion.inicio)," segundos")



######## Frecuencia de 30


X <- ts(datos.ts.entrenamiento$Compra, frequency = 30)


#ts.real<-ts(datos.ts.prueba$Compra,frequency = 7,start = c(18, 4) )

# 
# fixed <- c(
#   
#   NA,NA,NA,0,NA,NA, # Coeficientes AR
#   NA,NA,NA,0,NA,NA, # Coeficientes MA
#   NA,0,NA,NA,NA,NA,NA,NA,NA,0,NA,0,0, # Coeficientes SAR
#   NA,NA,NA,NA,NA,NA,NA,NA,NA,0,NA,0  # Coeficientes SMA
# )



#### Sin Ajuste

# Corte Junio
# modelo.sarima.freq30<- forecast::Arima(X, order = c(p.30,1,q.30)
#                                      , seasonal = c(P.30,0,Q.30)
#                                      #,include.drift = T
#                                      , lambda = 0
#                                      #, fixed = fixed
# )

## Corte Setiembre
modelo.sarima.freq30<- forecast::Arima(X, order = c(p.30-1,1,q.30)
                                       , seasonal = c(P.30,0,Q.30)
                                       #,include.drift = T
                                       , lambda = 0
                                       #, fixed = fixed
)

fixed <- rep(NA, length(modelo.sarima.freq30$coef))

tiempo.ejecucion.inicio<-Sys.time()

# modelo.sarima.freq7<- forecast::Arima(X, order = c(p.30,1,q.30)
#                                       , seasonal = c(P.30,0,Q.30)
#                                       , lambda = 0
#                                       , fixed = fixed
# )

tiempo.ejecucion.final<-Sys.time()


# Revision del AIC 
summary(modelo.sarima.freq30)

aic.modelo.sarima.freq30<- round(summary(modelo.sarima.freq30)$aic)

print(paste0("El AIC del modelo es de: ", aic.modelo.sarima.freq30))

#"El AIC del modelo es de: -32464" sin ajuste

# "El AIC del modelo es de: -32539" con ajuste


# Revision de raices inversas 
plot(modelo.sarima.freq30)



# Revision de siginificancia de los coeficientes
summary_arima(modelo.sarima.freq30, fixed = fixed)


# Prueba de normalidad de los residuos 

# H0: residuos normales 
# H1: residuos no normales

ks.test(scale(c(modelo.sarima.freq30$residuals)), "pnorm")

# p-value = 2.2e-16 se rechaza H0 


# Homocedasticidad 

# H0: residuos Homcedasticos 
# H1: residuos no homocedasticos 
lmtest::bptest(lm(modelo.sarima.freq30$residuals~ time(modelo.sarima.freq30$residuals)))


# p-value = 2.2e-16 se rechaza H0 

# Revisar valores Ljung 
#TS.diag(c(modelo.arima.freq7$residuals),lag= 40)


LSTS::ts.diag(c(modelo.sarima.freq30$res), lag = 150)[[3]]

# Pasa el test blancura 

# Visualizacion de los datos 
par(mfrow=c(1,1))

#### Pronosticos 


pred.modelo.sarima.freq30<- forecast::forecast(modelo.sarima.freq30, h = intervalos.pred+1)


# Revision Visual de ajuste de los residuos 
plot(X)
lines(pred.modelo.sarima.freq30$fitted, col = "red")

plot(X
     , xlim = c((max(indices.entrenamiento)-revision.backtesting)/30,(max(indices.entrenamiento))/30)
     , ylim = c(495,530)
     , xlab="Periodo de Tiempo en días"
     , ylab = " Tipo de Cambio"
     , main = paste("Prueba residuos fitted desde ", fecha.backtesting , " hasta ", dia.corte)
)
lines(pred.modelo.sarima.freq30$fitted, col = "red")



# Revision visual de la Prediccion 

margen.dias<-50/30


plot(pred.modelo.sarima.freq30
     ,xlim=c((min(indices.test)-margen.dias)/30,(max(indices.test))/30)
     ,ylim=c(490,580)
     , xlab="Periodo de Tiempo en meses"
     , ylab = "Tipo de Cambio"
     , main = paste("Prueba backtesting pronóstico desde ", dia.corte , " hasta ", fecha.prediccion)
     
)
#lines(indices.test/7, datos.ts.prueba$Compra, col = "red")
lines(as.numeric(time(pred.modelo.sarima.freq30$mean)), datos.ts.prueba$Compra, col = "red")

### Calculo MAPE 

mape.modelo.sarima.freq30<- mean(abs(pred.modelo.sarima.freq30$mean/datos.ts.prueba$Compra-1))*100

paste("El Mape del modelo es: ", round(mape.modelo.sarima.freq30,2), "%")

paste("El tiempo de ejecucion del modelo es de ", round(tiempo.ejecucion.final-tiempo.ejecucion.inicio)," segundos")



#######  SARIMA Propuesto por Dr. Ricardo Olea UCC


X <- ts(datos.ts.entrenamiento$Compra, frequency = 7)


#ts.real<-ts(datos.ts.prueba$Compra,frequency = 7,start = c(18, 4) )


# modelo.sarima.rolea<- forecast::Arima(X, order = c(1,1,13)
#                                       , seasonal = c(2,0,4)
#                                       , lambda = 0
#                                       #, fixed = fixed
# )




fixed <- fixed <- c(
  
  NA, # Coeficientes AR
  NA,0,NA,NA,0,NA,0,NA,NA,0,0,0,NA, # Coeficientes MA
  NA,NA, # Coeficientes SAR
  NA,NA,0,NA # Coeficientes SMA
  
  )

tiempo.ejecucion.inicio<-Sys.time()

modelo.sarima.rolea<- forecast::Arima(X, order = c(1,1,13)
                                      , seasonal = c(2,0,4)
                                      , lambda = 0
                                      , fixed = fixed
)

tiempo.ejecucion.final<-Sys.time()


# Revision del AIC 
summary(modelo.sarima.rolea)

aic.modelo.sarima.rolea<- round(summary(modelo.sarima.rolea)$aic)

print(paste0("El AIC del modelo es de: ", aic.modelo.sarima.rolea))

#"El AIC del modelo es de: -32469" sin ajuste

# "El AIC del modelo es de: -32472" con ajuste


# Revision de raices inversas 
plot(modelo.sarima.rolea)



# Revision de siginificancia de los coeficientes
summary_arima(modelo.sarima.rolea, fixed = fixed)


# Prueba de normalidad de los residuos 

# H0: residuos normales 
# H1: residuos no normales

ks.test(scale(c(modelo.sarima.rolea$residuals)), "pnorm")

# p-value = 2.2e-16 se rechaza H0 


# Homocedasticidad 

# H0: residuos Homcedasticos 
# H1: residuos no homocedasticos 
lmtest::bptest(lm(modelo.sarima.rolea$residuals~ time(modelo.sarima.rolea$residuals)))


# p-value = 2.2e-16 se rechaza H0 

# Revisar valores Ljung 
#TS.diag(c(modelo.arima.freq7$residuals),lag= 40)


LSTS::ts.diag(c(modelo.sarima.rolea$res), lag = 90)[[3]]

# Pasa el test blancura 

# Visualizacion de los datos 
par(mfrow=c(1,1))

#### Pronosticos 


pred.modelo.sarima.rolea<- forecast::forecast(modelo.sarima.rolea, h = intervalos.pred+1)


# Revision Visual de ajuste de los residuos 
plot(X)
lines(pred.modelo.sarima.rolea$fitted, col = "red")

plot(X
     , xlim = c((max(indices.entrenamiento)-revision.backtesting)/7,(max(indices.entrenamiento))/7)
     , ylim = c(495,530)
     , xlab="Periodo de Tiempo en días"
     , ylab = " Tipo de Cambio"
     , main = paste("Prueba residuos fitted desde ", fecha.backtesting , " hasta ", dia.corte)
)
lines(pred.modelo.sarima.rolea$fitted, col = "red")



# Revision visual de la Prediccion 

margen.dias<-50/7


plot(pred.modelo.sarima.rolea
     ,xlim=c((min(indices.test)-margen.dias)/7,(max(indices.test))/7)
     ,ylim=c(490,580)
     , xlab="Periodo de Tiempo en semanas"
     , ylab = "Tipo de Cambio"
     , main = paste("Prueba backtesting pronóstico desde ", dia.corte , " hasta ", fecha.prediccion)
     
)
#lines(indices.test/7, datos.ts.prueba$Compra, col = "red")
lines(as.numeric(time(pred.modelo.sarima.rolea$mean)), datos.ts.prueba$Compra, col = "red")


LSTS::ts.diag(c(modelo.sarima.rolea$res^2), lag = 21)[[3]]
### Calculo MAPE 

mape.modelo.sarima.rolea<- mean(abs(pred.modelo.sarima.rolea$mean/datos.ts.prueba$Compra-1))*100

paste("El Mape del modelo es: ", round(mape.modelo.sarima.rolea,2), "%")
# "El Mape del modelo es:  1.38 %"
paste("El tiempo de ejecucion del modelo es de ", round(tiempo.ejecucion.final-tiempo.ejecucion.inicio)," segundos")



###### MODELOS SARIMA-GARCH

sarima.res.rolea <- residuals(modelo.sarima.rolea)


sarima.garch.rolea <- fGarch::garchFit(formula = arma(6,6)~ garch(5,1), 
                      data = sarima.res.rolea, 
                      trace = FALSE)

# plot(sarima.garch.rolea)
summary(sarima.garch.rolea)



# Revision de siginificancia de los coeficientes
summary_arima(modelo.sarima.rolea, fixed = fixed)


# Prueba de normalidad de los residuos 

# H0: residuos normales 
# H1: residuos no normales

ks.test(scale(c(residuals(modelo.sarima.rolea))), "pnorm")

# p-value = 2.2e-16 se rechaza H0 


# Homocedasticidad 

# H0: residuos Homcedasticos 
# H1: residuos no homocedasticos 
lmtest::bptest(lm(residuals(sarima.garch.rolea)~ time(residuals(sarima.garch.rolea))))


# p-value = 2.2e-16 se rechaza H0 

# Revisar valores Ljung 
#TS.diag(c(modelo.arima.freq7$residuals),lag= 40)


LSTS::ts.diag(c(residuals(sarima.garch.rolea)), lag = 90)[[3]]

LSTS::ts.diag(c(residuals(sarima.garch.rolea)^2), lag = 21)[[3]]


# H0: El modelo captura todas las autocorrelaciones y solo hay ruido blanco
# H1: El modelo aun puede ajustarse mejorar
Box.test(residuals(sarima.garch.rolea)^2, lag = 21, type = "Ljung-Box")


# construccion del modelo spec para pronosticos 
sarima.garch.rolea.coeficientes<-as.list(coef(sarima.garch.rolea))
set.seed(123)
sarima.garch.rolea.spec = garchSpec(model = sarima.garch.rolea.coeficientes
                                                 , cond.dist = "snorm",rseed = 13)

# simulacion del mejor modelo arima arch para poder hacer pronosticos 
set.seed(124)
sarima.garch.rolea.simulacion <- garchSim(spec = sarima.garch.rolea.spec, n = intervalos.pred+1)
plot(sarima.garch.rolea.simulacion)


ultimo.tipo.cambio<-as.numeric(tail(datos.ts.entrenamiento, n=1)["Compra"])
sarima.garch.rolea.pronostico.valores<- ultimo.tipo.cambio * apply(sarima.garch.rolea.simulacion, 2, 'cumsum') + ultimo.tipo.cambio

plot(sarima.garch.rolea.pronostico.valores,type = "l",
     , ylim = c(495,530)
     #,lwd = 3
     ,xlab='Observaciones',ylab='Tipo de Cambio')
lines(as.numeric(time(sarima.garch.rolea.pronostico.valores))
      , datos.ts.prueba$Compra, col = "red")
class(sarima.garch.rolea.pronostico.valores)

mape.modelo.sarima.garch.rolea<- mean(abs(sarima.garch.rolea.pronostico.valores/datos.ts.prueba$Compra-1))*100

paste("El Mape del modelo es: ", round(mape.modelo.sarima.garch.rolea,2), "%")
# [1] "El Mape del modelo es:  1.04 %"



################## rugarch 

X <- ts(diff(log(datos.ts.entrenamiento$Compra)))
plot(X)
#X<-X[-1] # se producira un error


# Specify the GARCH model
modelo.sarima.garch.spec <- ugarchspec(
  variance.model = list(garchOrder = c(1,2),model = "sGARCH" ),
  mean.model = list(armaOrder = c(5, 6), include.mean = TRUE),
  distribution.model = "norm",
  
)

# Fit the model
fit <- ugarchfit(spec = modelo.sarima.garch.spec, data = X)

fit

### El fit nos da que podemos considerar los residuos cuadrados
### se comportan como ruido
# 
plot(fit, which = 1) # 2 stds 
plot(fit, which = 2) # 2 stds 
plot(fit, which = 3) # 2 stds 
plot(fit, which = 4) # 2 stds 
plot(fit, which = "all") 


# creacion de simulaciones de varianza del modelo 

sim <- ugarchsim(
  fit = fit,
  n.sim = intervalos.pred + 1,
  m.sim = 100,
  rseed = 123
)


ultimo.tipo.cambio<-tail(datos.ts.entrenamiento$Compra,1)

tipo.cambio.garch<-ultimo.tipo.cambio*apply(fitted(sim),2,'cumsum')+ultimo.tipo.cambio

# Todos las simulaion
matplot(tipo.cambio.garch,type = "l")

media.pronostico.garch <- rowMeans(tipo.cambio.garch)
ic.95.bajo <- apply(tipo.cambio.garch, 1, quantile, probs = 0.025)
ic.95.alto <- apply(tipo.cambio.garch, 1, quantile, probs = 0.975)
ic.80.bajo <- apply(tipo.cambio.garch, 1, quantile, probs = 0.1)
ic.80.alto <- apply(tipo.cambio.garch, 1, quantile, probs = 0.9)

plot(media.pronostico.garch, type="l", ylim=c(510,528) , ylab="Tipo de cambio", xlab="Pronóstico en días"
     , main = paste("Prueba backtesting pronóstico desde ", dia.corte , " hasta ", fecha.prediccion))
lines(ic.95.bajo, col = "red", lty = 2)
lines(ic.95.alto, col = "red", lty = 2)
lines(ic.80.bajo, col = "blue", lty = 2)
lines(ic.80.alto, col = "blue", lty = 2)
lines(datos.ts.prueba$Compra,col = "green" )

mape.arima.garch<- mean(abs(media.pronostico.garch/datos.ts.prueba$Compra-1))*100

paste("El Mape del modelo es: ", round(mape.arima.garch,2), "%")

# Mape del modelo es de 1.74 %


################# Construccion de Data Frame resumen 


resultados.modelo.total <- data.frame(
   fecha = dia.corte+seq(0,length(datos.ts.prueba$Compra)-1,1)
  ,tipo.cambio.real = datos.ts.prueba$Compra
  ,tipo.cambio.arima.freq7 = pred.modelo.arima.freq7$mean
  ,tipo.cambio.arima.freq30 = pred.modelo.arima.freq30$mean
  ,tipo.cambio.sarima.freq7 = pred.modelo.sarima.freq7$mean
  ,tipo.cambio.sarima.freq30 = pred.modelo.sarima.freq30$mean
  ,tipo.cambio.sarima.rolea = pred.modelo.sarima.rolea$mean
  ,tipo.cambio.arima.garch = media.pronostico.garch
)

resultados.mape.modelo.total <- data.frame(
  nombre.modelo =  colnames(resultados.modelo.total[,-c(1,2)]),
  valores = round(c(mape.modelo.arima.freq7,
              mape.modelo.arima.freq30,
              mape.modelo.sarima.freq7,
              mape.modelo.sarima.freq30,
              mape.modelo.sarima.rolea,
              mape.arima.garch),3)
  
)

resultados.mape.modelo.total$precision<-100-resultados.mape.modelo.total$valores

resultados.mape.modelo.total[order(resultados.mape.modelo.total$precision
                                   , decreasing = TRUE), ]


###  Grafica de pronosticos 
matplot(resultados.modelo.total$fecha,resultados.modelo.total[,-c(1)]
        , type = "l"
        , col = 1:ncol(resultados.modelo.total[,-c(1)])
        , lty = 1:ncol(resultados.modelo.total[,-c(1)])
        ,xlab=""
        ,ylab="Tipo de cambio")

# Add legend
legend(
  "topleft",                           
  legend = colnames(resultados.modelo.total[,-c(1)]),  
  col = 1:ncol(resultados.modelo.total[,-c(1)]),       
  lty = 1:ncol(resultados.modelo.total[,-c(1)]),                                              
  cex = 0.8,
  xpd = TRUE,
  inset = c(0, -0.3),
  ncol = 3
  
)


#### Analisis Mes a Mes 

Mes.a.Mes <- datos.tipo.cambio[format(datos.tipo.cambio$DateDt, "%d") == "01"
                               ,c("DateDt","Compra","Venta")]


Mes.a.Mes$Cambio.Abs.Compra<-c(NA, diff(Mes.a.Mes$Compra))
Mes.a.Mes$Cambio.Abs.Venta<-c(NA, diff(Mes.a.Mes$Venta))


Mes.a.Mes$Cambio.Rel.Compra <-Mes.a.Mes$Cambio.Abs.Compra/(Mes.a.Mes$Compra-Mes.a.Mes$Cambio.Abs.Compra)*100
Mes.a.Mes$Cambio.Rel.Venta<-Mes.a.Mes$Cambio.Abs.Venta/(Mes.a.Mes$Venta-Mes.a.Mes$Cambio.Abs.Venta)*100

Mes.a.Mes$Spread <-abs(Mes.a.Mes$Compra-Mes.a.Mes$Venta)

## Spread
plot(Mes.a.Mes$DateDt,Mes.a.Mes$Spread, type="l"
     , xlab="Año", ylab="Spread en colones", main="Cambio del spread mensual entre compra y venta")

# Rentabilidad
plot(Mes.a.Mes$DateDt,Mes.a.Mes$Cambio.Rel.Compra, type="l"
     , xlab="Año", ylab="Cambio Porcentual%", main="Cambio porcentual mensual")
abline(h=0,col = "red")
abline(h=1,col = "green")


# Distribucion de Rendimientos Porcentuales
hist(Mes.a.Mes$Cambio.Rel.Compra,breaks = 50, xlim = c(-2,2)
     , main="Histograma rendimientos porcentuales tipo cambio compra"
     , xlab="Cambio porcentual"
     , ylab = "Frecuencia")


#### Analisis Anio a Anio 

Ano.a.Ano <- datos.tipo.cambio[format(datos.tipo.cambio$DateDt, "%m") == "01" & 
                              format(datos.tipo.cambio$DateDt, "%d") == "01"
                               ,c("DateDt","Compra","Venta")]


Ano.a.Ano$Cambio.Abs.Compra<-c(NA, diff(Ano.a.Ano$Compra))
Ano.a.Ano$Cambio.Abs.Venta<-c(NA, diff(Ano.a.Ano$Venta))


Ano.a.Ano$Cambio.Rel.Compra <-Ano.a.Ano$Cambio.Abs.Compra/(Ano.a.Ano$Compra-Ano.a.Ano$Cambio.Abs.Compra)*100
Ano.a.Ano$Cambio.Rel.Venta<-Ano.a.Ano$Cambio.Abs.Venta/(Ano.a.Ano$Venta-Ano.a.Ano$Cambio.Abs.Venta)*100


Ano.a.Ano$Spread <-abs(Ano.a.Ano$Compra-Ano.a.Ano$Venta)

plot(Ano.a.Ano$DateDt,Ano.a.Ano$Cambio.Rel.Compra, type="l"
     , xlab="Año", ylab="Cambio Porcentual")





############################################## PRONOSTICOS 2025 ##############





#### MEDIANO PLAZO ##### 

# determinar el area de trabajo 
setwd('/Users/fkkarpuk/Desktop/Projects/PresentacionTipoCambio')


prueba.final<-rio::import('PruebaOctubre.csv')
reservas<-rio::import('Reservas.csv')

plot(reservas$DateDt[reservas$DateDt >= as.Date('2015-01-01') & reservas$DateDt <= as.Date('2025-01-01')],
     reservas$Reserves[reservas$DateDt >= as.Date('2015-01-01') & reservas$DateDt <= as.Date('2025-01-01')] / 1e6,
     type = "l", 
     main = "Reservas del Banco Central de 2015 a 2025", 
     xlab = "Año", 
     ylab = "Reservas en Millones de Dólares")


X <- ts(datos.tipo.cambio$Compra)

#ts.real<-ts(datos.ts.prueba$Compra,frequency = 7)


fixed <- c(NA,0,NA,NA,NA,NA # Coeficientes AR 
           ,NA,0,NA,NA,NA,0 # Coeficientes MA
)



modelo.med.plazo<- forecast::Arima(X, order = c(p.7,1,q.7)
                                     #, seasonal = c(2,0,4)
                                     , lambda = 0
                                     , fixed = fixed
)


pred.med.plazo<-150
primer.dia.pronostico.med.plazo<-as.Date(tail(datos.tipo.cambio$DateDt,1)+1)

pred.modelo.med.plazo<- forecast::forecast(modelo.med.plazo, h = pred.med.plazo)


plot(pred.modelo.med.plazo , xlim=c(3564-60, 3564+pred.med.plazo), ylim=c(505,530))

## Construccion de pronostico

pronostico.med.plazo<- as.data.frame(pred.modelo.med.plazo)


pronostico.med.plazo.final<- data.frame(
       fecha=1:nrow(pronostico.med.plazo)+primer.dia.pronostico.med.plazo-1,
       Pronostico.esperado = pronostico.med.plazo$`Point Forecast`,
       Pronostico.limite.alto = pronostico.med.plazo$`Hi 95`
)

cbind(pronostico.med.plazo.final,)
pronostico.med.plazo.final<-merge(prueba.final,  pronostico.med.plazo.final, by.x = "DateDt", by.y = "fecha", all.y = TRUE)

# Plot the first line (Pronostico.limite.alto)
plot(pronostico.med.plazo.final$DateDt, 
     pronostico.med.plazo.final$Pronostico.limite.alto, 
     type = "l", 
     main = "Pronóstico a Mediano Plazo", 
     xlab = "Fecha", 
     ylab = "Valores", 
     ylim = range(c(pronostico.med.plazo.final$Pronostico.limite.alto, 
                    pronostico.med.plazo.final$Pronostico.esperado)))

lines(pronostico.med.plazo.final$DateDt, 
      pronostico.med.plazo.final$Pronostico.esperado, 
      col = "red")

lines(pronostico.med.plazo.final$DateDt, 
      pronostico.med.plazo.final$ExchangeRateBid, 
      col = "blue")




lines(pronostico.med.plazo.final$Pronostico.esperado, col="red")
plot(pronostico.med.plazo.final$fecha,pronostico.med.plazo.final$Pronostico.esperado, type="l")


margen.dias<-50


plot(modelo.med.plazo
     #,xlim=c(min(n.med.plazo)-margen.dias,max(n.med.plazo))
     ,ylim=c(490,580)
     , xlab="Periodo de Tiempo en días"
     , ylab = "Tipo de Cambio"
     , main = paste("Prueba backtesting pronóstico desde ", dia.corte , " hasta ", fecha.prediccion)
     
)
lines(indices.test, datos.ts.prueba$Compra, col = "red")


### Calculo MAPE 

mape.modelo.med.plazo<- mean(abs(pred.modelo.med.plazo$mean/datos.ts.prueba$Compra-1))*100

paste("El Mape del modelo es: ", round(mape.modelo.med.plazo,2), "%")





#### Corto Plazo ##### 


X <- ts(datos.tipo.cambio$Compra)


#ts.real<-ts(datos.ts.prueba$Compra,frequency = 7,start = c(18, 4) )


fixed <- c(
  # Coeficientes AR
  NA,NA,NA,NA,NA,NA,NA,NA,0,0,NA,0,0,NA,NA,NA,NA,0,NA,NA,NA,NA,0,0,NA,0,0,NA, 
  # Coeficientes MA
  NA,NA,NA,NA,NA,NA,NA,NA,0,0,NA,0,0,NA,NA,NA,NA,0,NA,NA,NA,NA,NA,0,NA,0,0,NA
)



modelo.corto.plazo<- forecast::Arima(X, order = c(p.30,1,q.30)
                                      #, seasonal = c(P.7,0,Q.7)
                                      , lambda = 0
                                      , fixed = fixed
)


pred.corto.plazo<-40


primer.dia.pronostico.med.plazo<-as.Date(tail(datos.tipo.cambio$DateDt,1)+1)

pred.modelo.corto.plazo<- forecast::forecast(modelo.corto.plazo, h = pred.corto.plazo)


plot(pred.modelo.corto.plazo , xlim=c(3564-60, 3564+pred.corto.plazo), ylim=c(505,530))



pronostico.corto.plazo<- as.data.frame(pred.modelo.corto.plazo)


pronostico.corto.plazo.final<- data.frame(
  fecha=1:nrow(pronostico.corto.plazo)+primer.dia.pronostico.med.plazo-1,
  Pronostico.esperado = pronostico.corto.plazo$`Point Forecast`,
  Pronostico.limite.alto = pronostico.corto.plazo$`Hi 95`
)

pronostico.corto.plazo.final<-merge(prueba.final,  pronostico.corto.plazo.final, by.x = "DateDt", by.y = "fecha", all.y = TRUE)

# Plot the first line (Pronostico.limite.alto)
plot(pronostico.corto.plazo.final$DateDt, 
     pronostico.corto.plazo.final$Pronostico.limite.alto, 
     type = "l", 
     main = "Pronóstico a Corto Plazo", 
     xlab = "Fecha", 
     ylab = "Valores", 
     ylim = range(c(pronostico.corto.plazo.final$Pronostico.limite.alto, 
                    pronostico.corto.plazo.final$Pronostico.esperado)))

lines(pronostico.corto.plazo.final$DateDt, 
      pronostico.corto.plazo.final$Pronostico.esperado, 
      col = "red")

lines(pronostico.corto.plazo.final$DateDt, 
      pronostico.corto.plazo.final$ExchangeRateBid, 
      col = "blue")



