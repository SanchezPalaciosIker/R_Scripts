#Valuación de opciones

#Función Payoff Call (Ganancia bruta del contrato)
payoffCall <- function(S,K){ #S SUBYACENTE, K STRIKE
  if(S<K){
    p = 0 #Mejor no ejecutes en ese caso
  } else{
    p = S-K #Ejecuta el contrato y esta sería tu ganancia
  }
  return(p) #Return Payoff
}

#Ejemplo, precio pactado de compra es de 60, pero a ese tiempo
# el bien subyacente vale 100. Mejor ejecuto el contrato para comprar más barato 
# (y revender a precio mercado con ganancia)
payoffCall(100, 60)



#Función Payoff Put (Ganancia bruta del contrato)
payoffPut <- function(S,K){ #S SUBYACENTE, K STRIKE
  if(S<K){
    p = K-S #Ejecuta el contrato y esta es tu ganancia
  } else{
    p = 0 #Mejor no ejecutes en ese caso
  }
  return(p) #Return Payoff
}

#Ejemplo, precio pactado de venta es de 60, pero a ese tiempo
# el bien subyacente vale 100, mejor no ejecuto el contrato y vendo en el mercado
payoffPut(100, 60)


################################################################################

#Precio de una opción para tipo de cambio
#Considere el modelo de movimiento browniano para el peso mexicano
#Corra el código Movimiento_Browniano.R
d80 <- wt[80,] #Día 80 de todas las simulaciones
hist(d80, breaks = 20, freq = F)
mean(d80) #Promedio del día 80 de todas las simulaciones
sd(d80) 
mean(d80) + sd(d80) #Promedio más una desviación estándar


plot(wt[,1], type="l", ylim=c(min(wt), max(wt))) #Gráfica de la primera columna, i.e., gráfica del primer escenario
for (k in 2:m) {
  lines(wt[,k], col=k)
}


############################## PREDICCIÓN CALL POR SIMULACIÓN#################################
#Suponga que el precio pactado es de 18
abline(h=18, col = "red")

#Precio de la opción asociada al peso mexicano
K <- 18
gc <- c() #Vector de ganancias. quieres que en muchos escenarios tengas ganancias (gc es ganancias call)
for (k in 1:m) {
  gc[k] <- payoffCall(wt[n,k], K)
}

hist(gc) 
r <- 0.10 #Tasa libre de riesgo para llevar a valor presente el precio de la opción
prima <- mean(gc) #Media de las ganancias potenciales en el día 100
prima <- mean(gc)*exp(-r*n/360) #Prima a valor presente

#Si K = 10
K <- 10
gc <- c() #Vector de ganancias. quieres que en muchos escenarios tengas ganancias
for (k in 1:m) {
  gc[k] <- payoffCall(wt[n,k], K)
}

hist(gc) 
r <- 0.10 #Tasa libre de riesgo para llevar a valor presente el precio de la opción
prima <- mean(gc) #Media de las ganancias potenciales en el día 100
prima <- mean(gc)*exp(-r*n/360) #Prima a valor presente


#######################PREDICCIÓN PUT###########################################
K <- 18 
gp <- c()
for (k in 1:m) {
  gp[k] <- payoffPut(wt[n,k],K) 
}

hist(gp)
r <- 0.10
primap <- mean(gp)*exp(-r*n/360)


##############################Modelo de Black-Scholes para la valuación#########
BS<-function(S,K,sigma,r,t,op){
  t = t/360 #El tiempo se encuentra expresa en d?as 
  d1 = (log(S/K)+(r+sigma^2/2)*t)/(sigma*sqrt(t))
  d2 = d1-sigma*sqrt(t)
  
  if (op=="Call"){
    n1 = pnorm(d1)
    n2 = pnorm(d2)
    prima = S*n1 - K*exp(-r*t)*n2
  }
  if(op=="Put"){
    n1 = pnorm(-d1)
    n2 = pnorm(-d2)
    prima = K*exp(-r*t)*n2 - S*n1
  }
  return(prima)
}

#Observaremos diferencias entre ambas valuaciones
BS(d1$Close[length(d1$Close)], K, sigma, r, n, "Call") #Prácticamente vale 0 porque es dificil que el activo suba su precio
#Donde sigma = sigma <- sd(d1$Ren, na.rm = T) 

BS(d1$Close[length(d1$Close)], K, sigma, r, n, "Put") #Tiene valor porque es más facil que el activo disminuya 

