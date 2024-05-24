#Remuestreo mediante el algoritmo bootstraping: así funciona

#   10   5    3    8       (Pérdidas observadas)
# |----|----|----|----|    (Intervalos asociados a cada pérdida)
# 0   1/4   1/2  3/4  1    (Simulas un valor en el intervalo 0,1 y eliges pérdida asociada)

#Sirve para generar muestras aleatorias con una ponderación de este eestilo

#Modelos de VaR

#Librerias

library(quantmod)
library(dplyr)
library(tidyverse)
library(moments)
library(fitdistrplus)
library(goftest)
library(VGAM)
library(rriskDistributions)


# Datos

cartera <- c("^MXX","MXN=X","BTC-USD","NFLX")
getSymbols(cartera,src = "yahoo",from="2023-01-01",to="2023-11-20")

d1 <- as.data.frame(NFLX)
head(d1)
tail(d1)
names(d1) <- c("Open","High","Low","Close","Vol","Padj")
d1 <- dplyr::select(d1,c("Close"))


#Análisis gráfico de la serie
plot(d1$Close,type="l",col="blue")
boxplot(d1$Close)
summary(d1$Close)

#VaR Simulación Histórica

d1$Rend <- NA
d1$Reval <- NA
d1$PL <- NA


for (k in 2:length(d1$Close)) {
  d1$Rend[k] <- log(d1$Close[k]/d1$Close[k-1])
  d1$Reval[k] <- d1$Close[length(d1$Close)]*exp(+d1$Rend[k])
  d1$PL[k] <- d1$Close[length(d1$Close)]-d1$Reval[k]
}


plot(d1$Rend,type="l",col="blue")
abline(h=0,col="red")
abline(h=0.05,col="red")
abline(h=-0.05,col="red")

media <- mean(d1$PL,na.rm = T)
varSH <- quantile(d1$PL,c(0.90,0.95,0.99),na.rm=T)
varSH1 <- as.numeric(varSH[2])

hist(d1$PL,breaks = 50,freq = F,col="gray")
abline(v=media,col="red",lwd=3)
abline(v=varSH1,col="blue",lwd=3)

#El VaR nos indica la pérdida máxima que se tiene en el activo
#financiero con un nivel de confianza alpha y un horizonte de tiempo
#de un dia

#VaR Simulación Monte Carlo


d2 <- dplyr::select(d1,c("Close"))

d2$Rend <- NA
d2$RendSim <- NA
d2$Reval <- NA
d2$PL <- NA

for (k in 2:length(d2$Close)) {
  d2$Rend[k] <- log(d2$Close[k]/d2$Close[k-1])
}

#Estimadores por máxima verosimilitud de una densidad normal
mediaR <- mean(d2$Rend,na.rm = T)
sdR <- sd(d2$Rend,na.rm=T)

set.seed(30)
d2$RendSim <- rnorm(length(d2$Close),mediaR,sdR)

for (k in 2:length(d1$Close)) {
  d2$Reval[k] <- d2$Close[length(d1$Close)]*exp(+d2$RendSim[k])
  d2$PL[k] <- d2$Close[length(d1$Close)]-d2$Reval[k]
}


plot(d2$RendSim,col="blue",type="l")
abline(h=0,col="red")
abline(h=0.05,col="red")
abline(h=-0.05,col="red")


mediaSM <- mean(d2$PL,na.rm = T)
varSM <- quantile(d2$PL,c(0.90,0.95,0.99),na.rm = T)
varSM1 <- as.numeric(varSM[2])

hist(d2$PL,breaks = 50,freq = F,col="gray")
abline(v=mediaSM,col="red",lwd=3)
abline(v=varSM1,col="blue",lwd=3)

# VaR Lapalce

d3 <- dplyr::select(d1,c("Close","Rend"))

#Normal
rendN <- d3$Rend[!is.na(d3$Rend)]
hist(rendN,breaks = 50)

mean(rendN)
sd(rendN)
skewness(rendN) #moments E((x-E(x))^3)/sigma^3
kurtosis(rendN) #moments E((x-E(x))^4)/sigma^4


#Ajuste del modelo Normal
mod1 <- fitdist(rendN,"norm",method = "mle")

#Parametros
coef(mod1)[1]
coef(mod1)[2]

#Análisis gráfico de la distribucion
denscomp(mod1)
cdfcomp(mod1)
qqcomp(mod1)
ppcomp(mod1)

#Validacion del modelo
ks.test(rendN,"pnorm",mod1$estimate[1],mod1$estimate[2])
ad.test(rendN,"pnorm",mod1$estimate[1],mod1$estimate[2])

#Ajuste del modelo Laplce
mod2 <- fitdist(rendN,"laplace",method = "mle",start = list(location=0,scale=1))

#Parametros
coef(mod2)[1]
coef(mod2)[2]

#Análisis gráfico de la distribucion
denscomp(mod2)
cdfcomp(mod2)
qqcomp(mod2)
ppcomp(mod2)


#Validacion del modelo
ks.test(rendN,"plaplace",mod2$estimate[1],mod2$estimate[2])
ad.test(rendN,"plaplace",mod2$estimate[1],mod2$estimate[2])


#Por lo tanto, la distribucion de rendimeintos proviene de una
#densidad de Laplace o Doble Exponencial

#Comparativo de modelos
gofstat(list(mod1,mod2),fitnames = c("Normal","Laplace"))

fit.cont(rendN) #ajuste de varias fd


# Simulación con rendimeinto de Laplace
d3$RendSim <- NA
d3$Reval <- NA
d3$PL <- NA

set.seed(30)
d3$RendSim <- rlaplace(length(d3$Close),mod2$estimate[1],mod2$estimate[2])

for (k in 2:length(d3$Close)) {
  d3$Reval[k] <- d3$Close[length(d3$Close)]*exp(+d3$RendSim[k])
  d3$PL[k] <- d3$Close[length(d3$Close)]-d3$Reval[k]
}

plot(d3$RendSim,col="blue",type="l")
abline(h=0,col="red")
abline(h=0.05,col="red")
abline(h=-0.05,col="red")

mediaSM3 <- mean(d3$PL,na.rm = T)
varSM3 <- quantile(d3$PL,c(0.90,0.95,0.99),na.rm = T)
varSM3 <- as.numeric(varSM3[2])

hist(d3$PL,breaks = 50,freq = F,col="gray")
abline(v=mediaSM3,col="red",lwd=3)
abline(v=varSM3,col="blue",lwd=3)


#Simulación Bootstrap


d4 <- dplyr::select(d1,c("Close","Rend"))

set.seed(648) # semilla aleatoria
x <- seq(0,length(d3$Rend)-1,1)
s <- x/(length(x)-1)
u <- runif(length(d4$Rend)-1,0,1)
d4$RendBt <- NA

for (k in 1:(length(d3$Rend)-1)) {
  for (j in 1:(length(s)-1)){
    if(s[j]<u[k] & u[k]<s[j+1]){
      d4$RendBt[k+1] <- d4$Rend[j+1]
    } 
  }
}


hist(d4$RendBt,breaks = 50,freq = F,col="gray")

d4$Reval <- NA
d4$PL <- NA

for(k in 2:length(d4$Close)){
  d4$Reval[k] <- d4$Close[length(d4$Close)]*exp(d4$RendBt[k])
  d4$PL[k] <- d4$Close[length(d4$Close)]-d4$Reval[k]
}

par(mfrow=c(1,1))
plot(d4$Close,type = "l",col="red")
plot(d4$RendBt,type = "l",col="blue")

media <- mean(d4$PL,na.rm = TRUE)
var1 <- quantile(d4$PL,c(0.90,0.95,0.99,0.9990),na.rm = T)
var1 <- as.numeric(var1[3])

par(mfrow=c(1,1))
hist(d4$PL,breaks = 50,freq = F)
abline(v=media,col="blue",lwd = 3)
abline(v=var1,col="red",lwd = 3)



