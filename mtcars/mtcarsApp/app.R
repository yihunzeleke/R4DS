library(shinydashboard)
library(shiny)

library(shiny)
library(shinydashboard)
df <- mtcars
ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody()
)

server <- function(input, output) { }

shinyApp(ui, server)