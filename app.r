# app.R - Archivo Principal

require(shiny)
require(ggplot2)

source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)