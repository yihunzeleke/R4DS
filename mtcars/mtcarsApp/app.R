library(ggplot2)
library(dplyr)
library(shiny)
library(shinydashboard)

df <- mtcars

ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
        selectInput("X", "Select Names", choices = c("mpg","disp","hp","drat","qsec")),
        selectInput("Y", "Select Other ", choices = c("disp","drat","vs", "carb"))
    ),
    dashboardBody(
        fluidRow(
            plotOutput("distplot")
        )
    )
)

server <- function(input, output) {
    output$distplot <- renderPlot({
            ggplot(df,aes_string(x = input$X, y = input$Y))+
            geom_point()
            
    })
}

shinyApp(ui, server)

