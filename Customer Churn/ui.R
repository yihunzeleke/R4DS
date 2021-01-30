

library(shiny)
library(shinydashboard)
library(caTools)

train <- read.csv("train.csv", header = T)

#train <- train[,c(1,3,6,11)]


set.seed(100)

# Define UI for application 


ui <- dashboardPage(
    dashboardHeader(title = "Customer Churn Prediction"),
    dashboardSidebar(),
    dashboardBody(
        fluidRow(
            box(h3("Enter parameters"),
                sliderInput("ContractRenewal", label = "Contract Renewal", value = 0.47,
                            min = min(train$ContractRenewal),
                            max = max(train$ContractRenewal)),
                sliderInput("CustServCalls", label = "Customer Service Calls", value = 3,
                            min = min(train$CustServCalls),
                            max = max(train$CustServCalls)),
                sliderInput("RoamMins", label = "Roaming Minutes", value = 10.23,
                            min = min(train$RoamMins),
                            max = max(train$RoamMins)),
                
                actionButton("submitbutton", "Submit", class = "btn btn-primary",
                             icon("angle-double-down"))),
                
            box( tags$label(h3('Status/Output')), # Text-Box
                 verbatimTextOutput('contents'),
                 tableOutput('tbldata'), # prediction table
                
        )
    )
)
)  
    
    
    
    
    
# pageWithSidebar(
#     #page head
#     headerPanel("Custome Churn Prediction"),
#     
#     # input values
#     sidebarPanel(
#     HTML("<h3>Input parameters</h3>"),
#     
#     sliderInput("ContractRenewal", label = "Contract Renewal", value = 1,
#                 min = min(train$ContractRenewal),
#                 max = max(train$ContractRenewal)),
#     sliderInput("CustServCalls", label = "Customer Service Calls", value = 3,
#                 min = min(train$CustServCalls),
#                 max = max(train$CustServCalls)),
#     sliderInput("RoamMins", label = "Roaming Minutes", value = 10.23,
#                 min = min(train$RoamMins),
#                 max = max(train$RoamMins)),
#     
#     actionButton("submitbutton", "Submit", class = "btn btn-primary")
#     
#     
#     ),
#    
#     mainPanel(
#         tags$label(h3('Status/Output')), # Text-Box
#         verbatimTextOutput('contents'),
#         tableOutput('tbldata') # prediction table
#     )
# )

