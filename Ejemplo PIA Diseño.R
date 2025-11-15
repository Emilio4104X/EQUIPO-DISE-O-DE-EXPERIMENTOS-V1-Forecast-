# ======== CARGA DE LIBRERÍAS (con require) ========
require(shiny)
require(bslib)
require(forecast)

# ======== INTERFAZ DE USUARIO ========
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  
  titlePanel("Pronóstico de Series de Tiempo - Forecast App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Cargar archivo CSV", accept = ".csv"),
      numericInput("periodos", "Periodos a pronosticar:", 
                   value = 12, min = 1),
      actionButton("run", "Generar Forecast"),
      hr(),
      h4("Instrucciones:"),
      p("1. Carga un archivo CSV con una sola columna numérica."),
      p("2. Da clic en 'Generar Forecast'."),
      p("3. Observa la gráfica y el resumen del modelo.")
    ),
    
    mainPanel(
      h3("Gráfica del Forecast"),
      plotOutput("plot_forecast"),
      h3("Resumen del modelo ARIMA"),
      verbatimTextOutput("summary")
    )
  )
)

# ======== SERVIDOR ========
server <- function(input, output) {
  
  # Leer archivo
  datos <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Convertir a serie de tiempo
  serie <- eventReactive(input$run, {
    req(datos())
    ts(datos()[,1])
  })
  
  # Ajustar ARIMA automáticamente
  modelo <- reactive({
    req(serie())
    auto.arima(serie())
  })
  
  # Pronóstico
  prediccion <- reactive({
    req(modelo())
    forecast(modelo(), h = input$periodos)
  })
  
  # Gráfica
  output$plot_forecast <- renderPlot({
    req(prediccion())
    plot(prediccion(),
         main = "Pronóstico (Forecast)",
         ylab = "Valores",
         xlab = "Tiempo")
  })
  
  # Resumen del modelo
  output$summary <- renderPrint({
    req(modelo())
    summary(modelo())
  })
}

# ======== EJECUTAR APLICACIÓN ========
shinyApp(ui = ui, server = server)

