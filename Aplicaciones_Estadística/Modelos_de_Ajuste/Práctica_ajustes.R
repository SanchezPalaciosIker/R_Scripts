#install.packages(c("actuar", "stats", "moments", "readxl", "fitdistrplus", "goftest", "rriskDistributions"))

library(actuar) # distribuciones, modelos compuestos
library(stats) # funciones estadísticas
library(moments) # coeficientes de asimetría y curtosis
library(readxl) # Lectura de archivos .xlsx
library(fitdistrplus) # Ajustes de curvas
library(goftest) #Pruebas de bondad de ajuste
library(rriskDistributions) #Contraste general de modelos (No gamma, no burr, etc.)



######################## Ejemplo: V.A. Normal ##################################
m <- 10 #Media
s <- 5 #Desviación estándar
x <- m+2*s #Algún número en el soporte de la variable, por ejemplo, m+2*s
sim <- rnorm(100000, m, s) #Simulación


par(mfrow = c(2,3)) #Las siguientes gráficas se ordenarán en 3x3
curve(dnorm(x, m, s), xlim = c(m-3*s, m+3*s), col="red", lwd = 2, ylab="fx", main = "Densidad")
curve(pnorm(x, m, s), xlim = c(m-3*s, m+3*s), col = "blue", lwd = 2, ylab = "Fx", main = "Distribución acumulada")
curve(1-pnorm(x, m, s), xlim = c(m-3*s, m+3*s), col = "purple", lwd = 2, ylab = "Sx", main = "Supervivencia")
hist(sim, prob=T, main = "Simulación")
plot(ecdf(sim), main= "Distribución empírica")
qqnorm(sim, main = "Contraste de cuantiles" ); qqline(sim, col = "red")

############################ Ajuste de curvas ##################################

#1. Carga de datos
ruta <- "C:/Users/ASUS/Desktop/Reclamaciones.xlsx"
montos <- read_excel(path = ruta, sheet = "Montos", range = "A1:A627")
datos <- montos$Monto


#2. Estadística descriptiva
ed <- data.frame(
  Estadística = c('Tamaño muestral', 'Mínimo', 'Máximo', 'Rango', 'Mediana', 'Cuantil 0.25',
                  'Cuantil 0.50', 'Cuantil 0.75', 'Rango intercuantil', 'Promedio', 
                  'Varianza', 'Desviación estándar', 'Coeficiente de variación', 'Coeficiente de asimetría',
                  'Coeficiente de curtosis'),
  
  Valor = round(c(length(datos), min(datos) , max(datos), max(datos) - min(datos), median(datos), quantile(datos, 0.25),
            quantile(datos, 0.50), quantile(datos, 0.75), quantile(datos, 0.75) - quantile(datos, 0.25), mean(datos),
            var(datos), sd (datos), sd(datos)/mean(datos), skewness(datos), kurtosis(datos)), 2)
  
)

View(ed)

par(mfrow=c(1,3))
hist(datos, border = "purple", main = "Histograma de los datos") 
plot(ecdf(datos), col = "orange", main = "Distribución empírica" )
boxplot(datos, border = "steelblue", main = "Boxplot de los datos")


#3. Ajuste de curvas

#Por la pesadez de la cola en los datos, los modelos que tendrían sentido son 
# Weibull, Lognormal, Gamma, Pareto o Burr.

#Contraste general de modelos
fit.cont(datos)
# No se rechaza que los datos provengan de una distribución Lognormal.
# Así, un primer modelo plausible es la distribución Lognormal AIC = 11344.17; BIC = 11353.05
# Aún debemos revisar otras distribuciones posibles.

#Modelo Lognormal
mod0 <- fitdist(datos, "lnorm", method = "mle")
summary(mod0) # AIC = 11344.17; BIC = 11353.05


#Modelo Exponencial
mod1 <- fitdist(datos, "exp", method = "mme")
ks.test(datos, "pexp", mod1$estimate[1], mod1$estimate[2]) #Rechazada
ad.test(datos, "pexp", mod1$estimate[1], mod1$estimate[2]) #Rechazada


#Modelo Gamma
mod2 <- fitdist(datos, "gamma", method = "mme")
ks.test(datos, "pgamma", mod2$estimate[1], mod2$estimate[2]) #Rechazada
ad.test(datos, "pgamma", mod2$estimate[1], mod2$estimate[2]) #Rechazada


#Modelo Burr
mod3 <- fitdist(datos, "burr",method="mle",start=list(shape1=1,shape2=1,scale=100))
ks.test(datos, "pburr", shape1 = mod3$estimate[1], shape2 = mod3$estimate[2], scale = mod3$estimate[3]) #No rechazada
ad.test(datos, "pburr", shape1 = mod3$estimate[1], shape2 = mod3$estimate[2], scale = mod3$estimate[3]) #No rechazada
summary(mod3) # AIC = 11314.84 ; BIC = 11328.16 



#Contraste gráfico de candidatos
leyenda <-  c("Lognormal", "Burr")
par(mfrow = c(2, 2))
denscomp(list(mod0, mod3), legendtext = leyenda)
cdfcomp(list(mod0, mod3), legendtext = leyenda)
qqcomp(list(mod0, mod3), legendtext = leyenda)
ppcomp(list(mod0, mod3), legendtext = leyenda)

#Contraste por criterios Bayesiano y de Akaike
gofstat(list(mod0, mod3), fitnames = c("Lognormal", "Burr"))


#Elegimos el modelo Burr para los datos, pues BIC y AIC son menores que los del 
# modelo Lognormal




################################## NOTAS #######################################

#Tanto para el AIC como para el BIC se elige el modelo asociado al menor valor.
#El BIC es más usado. Penaliza más fuerte el número de parámetros en un modelo.
#"plausible" sólo significa que no se ha encontrado evidencia en contra.
#p-value >.05 no se rechaza H0. No hay evidencia en contra de H0.


### The Burr distribution with parameters shape1 = a, shape2 = b and scale = s has density:
### f(x) = (a b (x/s)^b)/(x [1 + (x/s)^b]^(a + 1))
### for x > 0, a > 0, b > 0 and s > 0.