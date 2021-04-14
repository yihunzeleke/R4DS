
#==# Library

library(shiny)
library(shinydashboard)
library(leaflet)
library(flexdashboard)
library(tmap)
library(ggpubr) # for getting color brewer get_palette()
library(plotly)
library(echarts4r)

#==# Data

source("C:/Users/yihun/Documents/COReview/OngoingProj/data/cleandata.R")

# pal <- colorNumeric(
#   palette = c(brewer.pal(4, "Blues")[c(1,2,3,4)]),
#   domain = zip_code_case$cases
# )
# palette = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), 4)

# pal = colorNumeric(
#   palette = get_palette('Oranges', 4),
#   domain = KCMO$cases
# )
# labels <-  dat$label <- with(dat, paste(
#   "<p> <b>", message, "</b> </br>",
#   date, "</br>",
#   "Count:", count,
#   "</p>"))

# https://nicovidtracker.org/



ui <- dashboardPage(
  dashboardHeader(title = "KCMO COVID19 Dashboard"),
  dashboardSidebar(
    sidebarMenu( 
      menuItem(strong("Cases"), tabName = "cov_case", icon = icon("plus")),
      #menuItem(strong("Hospitalizations"), tabName = "cov_hosp", icon = icon("hospital")),
      #menuItem(strong("Deaths"),  tabName = "cov_death", icon = icon("heart")),
      #menuItem(strong("Vaccine"), tabName = "cov_vax", icon = icon("vaccine")),
      menuItem(strong("Resources"), tabName =  "notes", icon = icon("pencil"))
    )
  ),
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "cov_case",
              fluidRow(
                box(title = h4(tags$em("Last Week Cases")), uiOutput("ov1"), background = "navy", width =4),
                box(title = h4(tags$em("Total Cases")), uiOutput("ov2"), background = "navy", width =4),
                box(title = h4(tags$em("Case Rates Per 100K")), uiOutput("ov3"), background = "navy", width =4)),
              fluidRow(
                box(title = "Cases by ZIP Code", tmapOutput("case_zip", height = 500),  width = 4,collapsible = T),
                box(title = "Case Rate Per 100K", tmapOutput("case_rate_zip", height = 500), width = 4, collapsible = T),
                box(title = "Positivity Rate", tmapOutput("positive_rate_zip", height = 500), width = 4,collapsible = T)),
              fluidRow(
                box(title = "Cumulative Number of Individuals Tested Positive", echarts4rOutput("case_commulative"), background = "navy")
              )
    ),
    tabItem(tabName = "notes",
            fluidRow(
              box(title = "Resources and Notes", uiOutput("technotes"))
            ),
            fluidRow(
              column(4, thumbnail_label(image = 'tweet.jpg', label = 'Application 1',
                                        content = 'Havana brown cornish rex bombay but bombay,
                                              but havana brown devonshire rex and devonshire rex.
                                              Tomcat egyptian mau. Cornish rex sphynx sphynx yet
                                              cougar and panther. Panther siberian. Lynx munchkin
                                              american shorthair. Norwegian forest. ',
                                        button_link = 'https://www.kcmo.gov/city-hall/departments/health/coronavirus', button_label = 'Click me')
              )
            )
            )
    )
    )
  )


server <- function(input, output, session) {
  #==# Cases
  output$ov1 <- renderUI({
    h3(cases %>% 
         mutate(Week = ceiling_date(date, "week")) %>% 
         group_by(Week) %>% 
         summarise(Cases = sum(new_cases)) %>% 
         slice_tail(n = 1) %>% pull(Cases))
  })
  
  output$ov2 <- renderUI({
    h3(formatC(cases %>% 
               summarise(Cases = sum(new_cases)) %>% pull(), digits = 0, format = "f", big.mark = ","))
  })
  
  output$ov3 <- renderUI({
    h3(formatC(cases %>% 
                 summarise(Cases = sum(new_cases)/4860404*100000)%>% pull(), digits = 2, format = "f", big.mark = ","))
  })

output$case_zip <- renderTmap({
    tm_shape(KCMO)+
      tm_polygons(col = "cases",id = 'id_case', palette = get_palette('Oranges', 6), title = "Number of Cases", 
                  style = "fixed",
                  breaks = c(0,100,500,1000,1500,Inf),
                  labels = c('100','500','1000','1500', "1500 and More")) +
      tm_layout()+
      tm_view(view.legend.position = c("left","bottom"))
  })
 
output$case_rate_zip <- renderTmap({
  tm_shape(KCMO)+
    tm_polygons(col = "Case Rates", id ="id_case_rate", palette = "YlOrRd",
                title = "Case Rate per 100k Residents",
                style = "fixed",
                breaks = c(0,1000,4500, 6750, 9000, Inf))+
    tm_layout()+
    tm_view(view.legend.position = c("left","bottom"))
  

})
output$positive_rate_zip <- renderTmap({
  tm_shape(KCMO)+
    tm_polygons(col = "Positivity Rate",id = "id_positive_rate", palette = "OrRd",
                style = "fixed",
                breaks = c(0, 10, 15, 20, 25,Inf),
                labels = c("10%", "15%", "20%", "25%", "25% or More"),
                title = "% Positivity Rate")+
                # popup.format= list(fun = function(x) paste0(formatC(x, digits = 0, format = "f", " Positivity Rate"))))+
    tm_layout()+
    tm_view(view.legend.position = c("left","bottom"))
  
})

output$case_commulative <- renderEcharts4r({
  cases %>% 
    rename(`Total Cases`= total_cases) %>% 
    e_chart(x = date) %>% 
    e_line(serie = `Total Cases`) %>%
    e_axis_labels(x = "Date") %>% 
    e_legend(show = F) %>% 
    e_tooltip(trigger = "axis") %>% 
    e_theme("chalk")
   
})
  #==# Technical notes 
  output$technotes <- renderUI({
    p(h3("Overview of KCMO Health Department COVID-19 Data", br()),
      h4(tags$li("COVID-19 data are reported as timely, as accurately,
                                             and as completely as we have available at the time of
                                          posting each business day. Data are updated as more information is
                                          received and will change over time as we learn more. ")),
      h4(tags$li("These metrics are updated on a daily basis, Monday-Friday: total cases,
                              case rate, total deaths, and death rate.")),
      h4(tags$li("All other metrics are updated on Wednesday for the prior week (Sunday-Saturday).
                              Notes on these metrics:")),
      tags$ul(
        h4(tags$li(tags$em("The 7-day daily case average reflects the prior week (Sunday-Saturday)"))),
        h4(tags$li(tags$em("the 14-day daily case average reflects the prior two weeks (Sunday-Saturday)"))),
        h4(tags$li(tags$em("the 30-day daily case average reflects the 30 days ending with Saturday of the prior week"))),
        h4(tags$li(tags$em("To account for delays in testing and reporting, the 14-day positivity rate is for the
                                      two-week period before the prior week. The prior week's positivity rate is shown on the chart
                                      but is not considered to be a final number.")))),
      h4(tags$li("For more information on the City's coronavirus/COVID-19 efforts, including current policies,
                              please visit:",
                 tags$a(href = "https://www.kcmo.gov/city-hall/departments/health/coronavirus","kcmo.gov/coronavirus"))))
  })
  
  
  output$site1 <- renderUI({
    
  })
 # <iframe height="400" width="100%" frameborder="no" src="https://datasciencegenie.shinyapps.io/MyShinyApp/"> </iframe>
}
shinyApp(ui, server)


  

})
output$positive_rate_zip <- renderTmap({
  tm_shape(KCMO)+
    tm_polygons(col = "Positivity Rate",id = "id_positive_rate", palette = "OrRd",
                style = "fixed",
                breaks = c(0, 10, 15, 20, 25,Inf),
                labels = c("10%", "15%", "20%", "25%", "25% or More"),
                title = "% Positivity Rate")+
                # popup.format= list(fun = function(x) paste0(formatC(x, digits = 0, format = "f", " Positivity Rate"))))+
    tm_layout()+
    tm_view(view.legend.position = c("left","bottom"))
  
})

output$case_commulative <- renderEcharts4r({
  cases %>% 
    rename(`Total Cases`= total_cases) %>% 
    e_chart(x = date) %>% 
    e_line(serie = `Total Cases`) %>%
    e_axis_labels(x = "Date") %>% 
    e_legend(show = F) %>% 
    e_tooltip(trigger = "axis") %>% 
    e_theme("chalk")
   
})
  #==# Technical notes 
  # output$notes <- renderUI({
  #   p(h3("Overview of KCMO Health Department COVID-19 Data", br()),
  #     h4(tags$li("COVID-19 data are reported as timely, as accurately, 
  #                                            and as completely as we have available at the time of 
  #                                         posting each business day. Data are updated as more information is
  #                                         received and will change over time as we learn more. ")),
  #     h4(tags$li("These metrics are updated on a daily basis, Monday-Friday: total cases,
  #                             case rate, total deaths, and death rate.")),
  #     h4(tags$li("All other metrics are updated on Wednesday for the prior week (Sunday-Saturday).
  #                             Notes on these metrics:")),
  #     tags$ul(
  #       h4(tags$li(tags$em("The 7-day daily case average reflects the prior week (Sunday-Saturday)"))),
  #       h4(tags$li(tags$em("the 14-day daily case average reflects the prior two weeks (Sunday-Saturday)"))),
  #       h4(tags$li(tags$em("the 30-day daily case average reflects the 30 days ending with Saturday of the prior week"))),
  #       h4(tags$li(tags$em("To account for delays in testing and reporting, the 14-day positivity rate is for the 
  #                                     two-week period before the prior week. The prior week's positivity rate is shown on the chart 
  #                                     but is not considered to be a final number.")))),
  #     h4(tags$li("For more information on the City's coronavirus/COVID-19 efforts, including current policies,
  #                             please visit:",
  #                tags$a(href = "https://www.kcmo.gov/city-hall/departments/health/coronavirus","kcmo.gov/coronavirus"))))
  # })

}
shinyApp(ui, server)

