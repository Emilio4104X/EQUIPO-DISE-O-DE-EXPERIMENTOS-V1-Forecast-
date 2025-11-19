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
      
      
      actionButton("run_regression", "Ejecutar Regresión", 
                   class = "btn-primary"),
      
      hr(),
      
      h4("Resultados del Modelo"),
      verbatimTextOutput("model_summary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Gráfico de Dispersión",
                 plotOutput("scatter_plot", height = "500px")),
        tabPanel("Diagnóstico",
                 plotOutput("diagnostic_plots", height = "600px")),
        tabPanel("Datos",
                 tableOutput("data_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  
  data <- reactive({
    if (input$data_source == "example") {
      mtcars
    } else {
      req(input$file)
      read.csv(input$file$datapath)
    }
  })
  
  output$x_var <- renderUI({
    df <- data()
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    selectInput("x", "Variable independiente (X):", 
                choices = numeric_vars,
                selected = numeric_vars[1])
  })
  
  output$y_var <- renderUI({
    df <- data()
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    selectInput("y", "Variable dependiente (Y):", 
                choices = numeric_vars,
                selected = numeric_vars[2])
  })
  
  model <- eventReactive(input$run_regression, {
    req(input$x, input$y)
    df <- data()
    formula <- as.formula(paste(input$y, "~", input$x))
    lm(formula, data = df)
  })
  
  output$model_summary <- renderPrint({
    req(model())
    m <- model()
    cat("Ecuación de regresión:\n")
    cat(sprintf("%s = %.4f + %.4f * %s\n\n", 
                input$y, coef(m)[1], coef(m)[2], input$x))
    cat("Resumen estadístico:\n")
    summary(m)
  })
  
  output$scatter_plot <- renderPlot({
    req(model())
    df <- data()
    
    ggplot(df, aes_string(x = input$x, y = input$y)) +
      geom_point(color = "steelblue", size = 3, alpha = 0.6) +
      geom_smooth(method = "lm", se = TRUE, color = "red", 
                  fill = "pink", alpha = 0.2) +
      labs(title = paste("Regresión Lineal:", input$y, "vs", input$x),
           x = input$x,
           y = input$y) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  output$diagnostic_plots <- renderPlot({
    req(model())
    par(mfrow = c(2, 2))
    plot(model())
  })
  
  output$data_table <- renderTable({
    head(data(), 20)
  })
}

shinyApp(ui = ui, server = server)