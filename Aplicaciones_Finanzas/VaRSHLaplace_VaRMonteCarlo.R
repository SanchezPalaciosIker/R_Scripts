install.packages("quantmod") #Para finanzas cuantitativas: getsymbols
install.packages("dplyr") #Para trabajar bases de datos
install.packages("tidyverse") #Para trabajar bases de datos
install.packages("moments") #Skewness
install.packages("fitdistrplus") #fitdist
install.packages("goftest") #ad.test y gofstat
install.packages("VGAM") #Distribución de Laplace (es una especie de doble exponencial)
install.packages("rriskDistributions") #fit.cont

library(quantmod)

#Datos
cartera <- c("^MXX", "MXN=X", "BTC-USD", "NFLX")
#Tenemos un índice, una moneda, una crypto y una acción, respectivamente
getSymbols(cartera, src = "yahoo", from = "2023-01-01", to = "2023-11-20") #Info de este año 

d1 <- as.data.frame(NFLX)
head(d1)
tail(d1)
names(d1) <- c("Open", "High", "Low", "Close", "Vol", "Padj") #Precio ajustado es Padj
#Hay que elegir con qué vector de precios quedarte para el análisis del VaR.
# En este caso, nos quedamos con el precio de cierre, Close

d1 <- dplyr::select(d1, c("Close"))

##################################Análisis#################################
#Primero se sugiere un análisis gráfico de la serie
plot(d1$Close, type ="l", col = "Blue") #Para ver cómo evoluciona la serie. Se muestra en este ejemplo una ganancia de aprox 100 dolares por acción

#Segundo un boxplot para identificar datos atípicos. Es un indicador de que algo podría salir mal en el modelo
boxplot(d1$Close) #En este caso, todo se ve bien
#En qué afectan los datos atípicos al análisis?
summary(d1$Close)
#Usar rendimientos continuos es lo más sano considerando la cotización diaria de los valores

###############################VaR Simulación histórica###############################

#Agregamos columnas a la tabla d1
d1$Rend <- NA
d1$Reval <- NA #Reevaluación del activo: VFuturo = VInicial exp(delta) "Suponer que el activo financiero para el siguiente día tendrá su ultimo precio*exp(+rendcont)
d1$PL <- NA #Positivo es pérdida. Negativo es ganancia. Es una variable aleatoria de pérdida "Cuánto dinero gané o perdí si ocurriera la reevaluación vs el último precio conocido"
View(d1)

#La reevaluación pretende proyectar en un día el precio del activo en función de los rendimientos continuos históricos

for (k in 2:length(d1$Close)){
  d1$Rend[k] <- log(d1$Close[k]/d1$Close[k-1]) #log(valorfinal/valorinicial)
  d1$Reval[k] <- d1$Close[length(d1$Close)]*exp(+d1$Rend[k])
  d1$PL[k] <- d1$Close[length(d1$Close)] - d1$Reval[k]
} #Ver foto 23/11/2023 8:11 pm excel


plot(d1$Rend, type = "l", col = "blue")
abline(h = 0, col = "red") #Línea en el 0
abline(h = 0.05, col = "red")
abline(h = -0.05, col = "red")

hist(d1$PL, breaks = 50, freq = F, col="gray") #Muestra cómo se ve la densidad de los rendimientos

media <- mean(d1$PL, na.rm = T) #No le hagas caso a los NA's
varSH <- quantile(d1$PL, c(0.90, 0.95, 0.99), na.rm=T) #Pérdida máxima en un horizonde de tiempo diario a los niveles de confianza escritos.
varSH1 <- as.numeric(varSH[2])


hist(d1$PL, breaks = 50, freq = F, col="gray") #Los breaks óptimos pueden determinarse por la regla de sturges
abline(v=media, col = "red", lwd = 3) #lwd es el tamaño de la línea
abline(v=varSH1, col = "blue", lwd = 3)
#Lado derecho están las pérdidas


#El VaR indica la pérdida máxima que tiene el activo financiero
#con un nivel de confianza alpha y un horizonte de tiempo de un día



###############################VaR Montecarlo###############################
#Hace un supuesto de normalidad sobre el comportamiento de los rendimientos
#Crearemos una tabla nueva para este VaR
library(moments)
d2 <- dplyr::select(d1, c("Close"))

d2$Rend <- NA
d2$RendSim <- NA #Rendimientos simulados
d2$Reval <- NA 
d2$PL <- NA 
View(d2)

#La reevaluación pretende proyectar en un día el precio del activo en función de los rendimientos continuos históricos

for (k in 2:length(d2$Close)){
  d2$Rend[k] <- log(d1$Close[k]/d1$Close[k-1]) 
}

#Estimadores por máxima verosimilitud de una densidad normal para los Rendimientos
mediaR <- mean(d2$Rend, na.rm=T)
sdR <- sd(d2$Rend, na.rm=T)


set.seed(40)
d2$RendSim <- rnorm(length(d2$Close), mediaR, sdR)

for (k in 2:length(d1$Close)){
  d2$Reval[k] <- d2$Close[length(d2$Close)]*exp(+d2$RendSim[k])
  d2$PL[k] <- d2$Close[length(d2$Close)] - d2$Reval[k]
}

plot(d2$RendSim, col = "blue", type="l")
abline(h = 0, col = "red") #Línea en el 0
abline(h = 0.05, col = "red")
abline(h = -0.05, col = "red")
#Es un VaR subestimado, porque reduce la volatilidad real del activo


mediaSM <- mean(d2$PL, na.rm = T)
varSM <- quantile(d2$PL, c(0.90, 0.95, 0.99), na.rm = T)
varSM1 <- as.numeric(varSM[2]) #var al 95%


hist(d2$PL,breaks = 50,freq = F,col="gray")
abline(v=mediaSM,col="red",lwd=3)
abline(v=varSM1,col="blue",lwd=3)


################VaR Laplace############################
d3 <- dplyr::select(d1, c("Close", "Rend"))

#Supuesto de normalidad
rendN <- d3$Rend[!is.na(d3$Rend)]
mean(rendN)
sd(rendN)
skewness(rendN) #Debería ser muy cercano a cero para asumir normalidad
kurtosis(rendN) #Debería ser 3 para ser una distribución normal de los datos. En este caso, no. 

library(fitdistrplus)

#Ajuste del modelo normal
mod1 <- fitdist(rendN, "norm", method = "mle") #Intenta ajustar una distribución normal a los rendimientos
coef(mod1)[1] #Media
coef(mod1)[2] #Sd
denscomp(mod1) #Esto compara gráficamente la densidad empírica vs la densidad del modelo que intentaste asociar
cdfcomp(mod1) #Todo hasta aquí va indicando que los rendimientos no se comportan como una distribución normal
qqcomp(mod1) #Los cuantiles empíricos vs teóricos también da indicios de que no provienen de una distribución normal
ppcomp(mod1) #La densidad empírica vs teórica también da indicios de no normalidad

#H0:= Los datos provienen de una distribución normal. Deseamos que los tests se rechacen
ks.test(rendN, "pnorm", mod1$estimate[1], mod1$estimate[2]) #el p-value rechaza normalidad en los rendimientos
ad.test(rendN, "pnorm", mod1$estimate[1], mod1$estimate[2]) #el p-value rechaza normalidad en los rendimientos

#Ajuste del modelo Laplace
library(VGAM)
mod2 <- fitdist(rendN, "laplace", method = "mle", start = list(location =0, scale = 1))
#Parametros
coef(mod2)[1]
coef(mod2)[2]

#Análisis gráfico de la distribucion
denscomp(mod2)
cdfcomp(mod2)
qqcomp(mod2)
ppcomp(mod2)
ks.test(rendN,"plaplace",mod2$estimate[1],mod2$estimate[2])
ad.test(rendN,"plaplace",mod2$estimate[1],mod2$estimate[2])
#No se rechaza H0, por lo que no existe evidencia en contra de 
#que la distribución de los rendimientos es de Laplace

#Contrastes de ajustes diferentes
library(goftest)
library(rriskDistributions)
gofstat(list(mod1, mod2), fitnames = c("Normal", "Laplace"))
fit.cont(mod1) #ajuste de varias densidades


#simulación con rendimientos de Laplace
#Es igualito al modelo montecarlo, pero simulando los rendimientos con la distribución de laplace

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





