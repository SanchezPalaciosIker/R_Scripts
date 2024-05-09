# 2. Sierpinski con n puntos iniciales ------------------------------------

# 2.1 Función para generar las etiquetas de cada punto --------------------

generar_letras <- function(N) {
  if (N <= 0) {
    stop("N debe ser un número entero positivo.")
  }
  
  # Función para generar la secuencia de letras
  generar_letra <- function(n) {
    if (n <= 26) {
      return(LETTERS[n])
    } else {
      primer_caracter <- LETTERS[(n - 1) %% 26 + 1]
      segundo_caracter <- LETTERS[((n - 1) %/% 26) %% 26 + 1]
      return(paste0(primer_caracter, segundo_caracter))
    }
  }
  
  # Aplicar la función generar_letra a la secuencia 1:N
  secuencia_letras <- sapply(1:N, generar_letra)
  
  return(secuencia_letras)
  
}




# 2.2 Algoritmo -----------------------------------------------------------


# Sea fig_x el vector con los valores abcisos de cada punto
# Sea fig_y el vector con los valores ordenados de cada punto
# Sea n el número de puntos por graficar

sierpinski <- function(fig_x , fig_y, n){
  
  if (length(fig_x) != length(fig_y)) {
    stop("Los vectores deben tener la misma longitud.")
  }
  
  etiquetas <- generar_letras(length(fig_x)) 
  colores <- rainbow(length(fig_x)) # Genera colores para cada punto
  names(colores) <- etiquetas # Asocia colores con etiquetas
  
  names(fig_x) <- etiquetas 
  names(fig_y) <- etiquetas 
  
  #Graficamos el cuadrado
  X11() # Esto abre un gráfico en una ventana nueva (opcional)
  plot.new() # Activa un gráfico nuevo
  
  llimx <- min(fig_x) - 0.5 # significa lower limit x
  ulimx <- max(fig_x) + 0.5 # significa upper limit x
  
  llimy <- min(fig_y) - 0.5
  ulimy <- max(fig_y) + 0.5
  
  plot.window(xlim = c(llimx, ulimx), ylim = c(llimy, ulimy)) # Delimita el plano cartesiano del gráfico
  points(x = fig_x, y = fig_y , lwd = 2, col = colores, pch = 16) #pch = style
  text(x = fig_x, y = fig_y, labels = etiquetas, pos = 1) #Das coordenadas donde quieres texto y texto
  
  
  
  
  # Iniciamos simulación
  
  #Primer punto medio inicial
  set.seed(1)
  sorteo <- sample(etiquetas,size=2,replace = FALSE) #Prob's default=unif(n)
  sorteo
  
  ptomed_rand <- c(sum(fig_x[sorteo])/2,
                   sum(fig_y[sorteo])/2)
  
  points(x=ptomed_rand[1], y = ptomed_rand[2], lwd = 2, col = 'gold', pch = 16)
  
  
  
  
  # Segundo punto medio, partiendo del punto medio inicial, en dirección aleatoria
  sorteo <- sample(etiquetas, size =1) #Dirección del siguiente punto
  ptomed_rand <- c((ptomed_rand[1]+fig_x[sorteo])/2,
                   (ptomed_rand[2]+fig_y[sorteo])/2)
  points(x = ptomed_rand[1], y = ptomed_rand[2],lwd=2, col = colores[sorteo], pch = 4)
  
  
  #Lo hacemos n veces
  n = 1000000
  
  for (i in 1:n){
    
    sorteo <- sample(etiquetas, size =1) #Dirección del siguiente punto
    ptomed_rand <- c((ptomed_rand[1]+fig_x[sorteo])/2,
                     (ptomed_rand[2]+fig_y[sorteo])/2)
    points(x = ptomed_rand[1], y = ptomed_rand[2],lwd=2, col = colores[sorteo], pch = 4)
    
  }
  
}




#Pentágono

x <- c(1.000000e+00,  3.090170e-01, -8.090170e-01, -8.090170e-01,  3.090170e-01) #Coord x
y <- c(0.0000000, 0.9510565, 0.5877853, -0.5877853, -0.9510565) # Coord y
n <- 1000 #Número de puntos

sierpinski(x, y, n)



# Romboide

x <- c(0.5,  1.5, -0.5, -1.5)
y <- c(0,  1,  2,  1)
n <- 1000
sierpinski(x, y, n)



# Hexágono 

x <- c(1.000000e+00,  5.000000e-01, -5.000000e-01, -1.000000e+00, -5.000000e-01,  5.000000e-01)
y <- c(0.000000e+00,  8.660254e-01,  8.660254e-01,  1.224647e-16, -8.660254e-01, -8.660254e-01)
n <- 1000
sierpinski(x, y, n)



# Pentadecágono
x <-  c(1.000000e+00, 9.659258e-01, 8.090170e-01, 5.877853e-01, 
        3.090170e-01, 6.123234e-17, -3.090170e-01, -5.877853e-01, 
        -8.090170e-01, -9.659258e-01, -1.000000e+00, -9.659258e-01, 
        -8.090170e-01, -5.877853e-01, -3.090170e-01)

y <- c(0.0000000, 2.5881905, 4.7552826, 6.1232340, 6.5463495, 6.1232340, 
       5.0000000, 3.2345896, 1.6209078, 0.2419219, -0.0000000, -0.2419219, 
       -1.6209078, -3.2345896, -5.0000000)

n <- 500
sierpinski(x, y, n)



# Octágono

x <- c(1.0000000, 0.7071068, 0.0000000, -0.7071068, -1.0000000, -0.7071068, 0.0000000, 0.7071068)
y <- c(0.0000000, 0.7071068, 1.0000000, 0.7071068, 0.0000000, -0.7071068, -1.0000000, -0.7071068)
n <- 600
sierpinski(x, y, n)
