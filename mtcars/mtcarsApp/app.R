library(ggplot2)
library(dplyr)
library(tidyverse)
library(magrittr)
library(shiny)
library(shinydashboard)

df_cars <- mtcars %>% 
    rownames_to_column(var ="Car") %>% 
    mutate(Car = strsplit(Car," ") %>% sapply(extract2, 1))
    


ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
        selectInput("X", "Select Names", choices = c("mpg","disp","hp","drat","qsec")),
        selectInput("Y", "Select Other ", choices = c("disp","drat","vs", "carb")),
        selectInput("car_table", "Select car Brand", choices = arrange(distinct(df_cars, Car)), selected = "Merc")
    ),
    dashboardBody(
        fluidRow(
            plotOutput("distplot"),
            tableOutput("tbl")
        )
    )
)

server <- function(input, output) {
    output$distplot <- renderPlot({
        df_cars %>% 
            filter(df_cars == input$car_table) %>% 
            ggplot(aes_string(x = input$X, y = input$Y))+
            geom_point()
            
    })
    
    output$tbl <- renderTable({
        df_cars %>% 
            filter(Car == input$car_table) %>% arrange(Car)
    })
}

shinyApp(ui, server)

