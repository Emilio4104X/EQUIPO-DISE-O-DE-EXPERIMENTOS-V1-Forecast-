# ui.R - Front End
# Ramiro Daniel Aguilar González - 2177783
require(shiny)
require(ggplot2)

ui <- fluidPage(
  titlePanel("Análisis de Regresión Lineal"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Configuración de Datos"),
      radioButtons("data_source", "Fuente de datos:",
                   choices = c("Datos de ejemplo (mtcars)" = "example",
                               "Cargar archivo CSV" = "upload")),
      
      conditionalPanel(
        condition = "input.data_source == 'upload'",
        fileInput("file", "Selecciona archivo CSV:",
                  accept = c(".csv"))
      ),
      
      uiOutput("x_var"),
      uiOutput("y_var"),
      
      hr(),
      # Cristofer Alejandro Alvarez Sandoval - 2100063
      h4("Opciones del Modelo"),
      
      checkboxInput("include_intercept", "Incluir intercepto en el modelo", value = TRUE),
      checkboxInput("log_transform", "Aplicar logaritmo a la variable dependiente (Y)", value = FALSE),
      
      sliderInput("conf_level", "Nivel de confianza:",
                  min = 0.80, max = 0.99, value = 0.95, step = 0.01),
      
      hr(),
      
      actionButton("run_regression", "Ejecutar Regresión", 
                   class = "btn-primary"),
      
      hr(),
      
      h4("Resultados del Modelo"),
      verbatimTextOutput("model_summary")
    ),
    #Alan Alberto González Mejía - 2103459   
    mainPanel(
      tabsetPanel(
        
        tabPanel("Gráfico de Dispersión",
                 plotOutput("scatter_plot", height = "500px")),
        
        tabPanel("Diagnóstico",
                 h4("Gráficos de diagnóstico del modelo"),
                 plotOutput("diagnostic_plots", height = "500px"),
                 hr(),
                 h4("Tabla de residuos"),
                 tableOutput("residuals_table")),
        
        tabPanel("Datos",
                 h4("Primeras 20 filas del conjunto de datos"),
                 tableOutput("data_table")),
        # Mauricio González Bermúdez - 2101442       
        # PREDICCIONES
        tabPanel("Predicciones",
                 h4("Generar predicciones"),
                 uiOutput("prediction_inputs"),
                 actionButton("predict_button", "Predecir"),
                 hr(),
                 verbatimTextOutput("prediction_text"),
                 tableOutput("prediction_table")),
        
        #  RESUMEN & CORRELACIONES 
        tabPanel("Resumen y Correlaciones",
                 h4("Resumen estadístico de las variables numéricas"),
                 tableOutput("data_summary"),
                 hr(),
                 h4("Matriz de correlaciones"),
                 tableOutput("correlation_table")),
        
        # COR.TEST 
        tabPanel("Correlación",
                 h4("Prueba de correlación de Pearson (cor.test)"),
                 verbatimTextOutput("correlation_test"))
      )
    )
  )
)