#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Convergencia de medias: Caso Weibull"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            numericInput(inputId = "shape",
                       "Parámetro de forma:",
                       value = 100, max = 100000),
          
            
            numericInput(inputId = "scale",
                         "Parámetro de escala:",
                         value = 100, max = 100000),
          
          
            sliderInput("n_sim",
                        "Número de simulaciones:",
                        min = 1,
                        max = 10000,
                        value = 100),
            
            
            sliderInput(inputId = "n_caminatas",
                        "Número de caminatas:",
                        min = 1,
                        max = 40,
                        value = 5),
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput(outputId = "caminatas")
        )
        
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # Generamos simulación con los inputs
      sh <- input$'shape'
      sc <- input$'scale'
      n <- input$'n_sim'
      simw <- rweibull(n = n, shape = sh, scale = sc)
      MASS::truehist(simw, col = 4)
        
    })
    
    
    
    output$caminatas <- renderPlot({
      
      
      
      # Construcción df ---------------------------------------------------------
      
      # Parámetros de la distribución Weibull
      sh <- input$'shape'
      sc <- input$'scale'
      
      # Media teórica para contrastar convergencia
      teo_mean <- sc * gamma(1+1/sh)
      
      t <- input$'n_caminatas'
      n <- input$'n_sim'
      n_caminata <- paste('Caminata', 1:t) %>% rep(each = n) # Columna Caminata_k
      
      #Construcción del dataframe
      df <- data.frame(n_caminata)
      
      
      # Columna: Índice (indica el número de simulaciones que promediará la caminata)
      indice <- rep(1:n, times = t) 
      df$indice <- indice 
      
      
      # Columna: Simulaciones Weibull
      x <- rweibull(n*t, shape = sh, scale = sc) 
      df$x <- x
      
      
      # Columna: Promedio de k<=n simulaciones (forma cada punto en la caminata)
      obs <- tapply(X = df$x, INDEX = df$n_caminata, FUN = cummean)
      obs <- unlist(obs)
      obs <- unname(obs)
      df$obs_caminata <- obs
      
      
      
      # Gráfica -----------------------------------------------------------------
      
      ggplot(data = df, 
             mapping = aes(x = indice, y = obs_caminata, col = n_caminata, group = n_caminata)) +
        geom_line()+
        geom_hline(yintercept = teo_mean, col = 'purple', lwd = 1.2) +
        labs(x = 'n', y = 'AVG(n)')
      
      
      
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
