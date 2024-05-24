#Caminatas aleatorias
p <- 0.45 #Probabilidad de avanza/subir
x <- c() #Posibles estados del proceso
x[1] <- 0 #Posición inicial del proceso
n <- 100 #Número de pasos del proceso

for (k in 2:n) {
  u <- runif(1,0,1) #simulación de un valor unif(0,1)
  if(u <= p) {x[k] <- x[k-1]+1} #El proceso avanza
  else {x[k] <- x[k-1]-1} #El proceso retrocede
}

plot(x, type = "l", col = "blue")
abline(h = 0, col = "red")


#Movimiento Browniano y P.E. de Weiner

#Proceso estocástico Aritmético para predicción de precios
library(quantmod)
library(dplyr)
library(tidyverse)
library(moments)

cartera <- c("^MXX","MXN=X","BTC-USD","NFLX")
getSymbols(cartera,src = "yahoo",from="2023-01-01",to="2023-11-20")

d1 <- as.data.frame(`MXN=X`)
head(d1)
tail(d1)
names(d1) <- c("Open","High","Low","Close","Vol","Padj")
d1 <- dplyr::select(d1,c("Close"))


#Cálculo de rendimientos
d1$Ren <- NA
for (k in 2:length(d1$Close)) {
  d1$Ren[k] <- log(d1$Close[k]/d1$Close[k-1])
}

plot(d1$Close, type="l") 
plot(d1$Ren, type="l")
abline(h=0, col="red")
abline(h=0.01, col="red")
abline(h=-0.01, col="red")

#Datos iniciales del modelo
s0 <- d1$Close[length(d1$Close)] #Último valor del tipo de cambio
mu <- mean(d1$Ren, na.rm = T) #Promedio de rendimientos, quitando NA's
sigma <- sd(d1$Ren, na.rm = T) #Desviación estándar de los rendimientos
t <- 1 #Tiempo que desea estimarse
st <- c() #Trayectoria que se sigue


#Predicción del activo financiero
set.seed(50)
for (k in 1:100) {
  if(k==1){
    st[1] <- s0 #Valor inicial del proceso
  } else{
    st[k] <- st[k-1] + st[k-1]*(mu*t+sigma*rnorm(1,0,1)*sqrt(t)) #rnorm es la simulación del movimiento browniano
    
  }
  
}
plot(st, type = "l", col="blue")


#Simulación del proceso para m escenarios
n <- 100 #Número de días que se estimaron en el proceso
m <- 50 #Número de escenarios
wt <- matrix(nrow = n , ncol = m) 

#For que llenará la matriz columna por columna
for (j in 1:m) { #número de escenarios
  for (k in 1:n) { #número de días de proyección
    if (k==1) {
      wt[k,j] <- s0
    }else{
      wt[k,j] <- wt[k-1,j] + wt[k-1,j]*(mu*t+sigma*rnorm(1,0,1)*sqrt(t))
    }
  }
}

plot(wt[,1], type="l", ylim=c(min(wt), max(wt))) #Gráfica de la primera columna, i.e., gráfica del primer escenario
for (k in 2:m) {
  lines(wt[,k], col=k)
}

abline(v=80, col = "red")


#Predicción del precio de un activo a un día determinado
d80 <- wt[80,] #Día 80 de todas las simulaciones
hist(d80, breaks = 20, freq = F)
mean(d80) #Promedio del día 80 de todas las simulaciones
sd(d80) 
mean(d80) + sd(d80) #Promedio más una desviación estándar

