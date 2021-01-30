
library(shiny)
library(data.table)
library(randomForest)

# load model
model <- readRDS("model.rds")

# Define server 
server <- function(input, output, session){
    
    # input data
    DataSetInput <- reactive({
        
        churn_df <- data.frame(
            Name = c("ContractRenewal",
                     "CustServCalls",
                     "RoamMins"),
            value = as.character(c(input$ContractRenewal,
                                   input$CustServCalls,
                                   input$RoamMins)),
            stringsAsFactors = F)
        
        Churn <- 0
        churn_df <- rbind(churn_df, Churn)
        
        input <- transpose(churn_df)
        write.table(input, "input.csv", sep = ",", quote = FALSE, row.names = FALSE, col.names = FALSE)
        
        test <- read.csv(paste("input",".csv", sep = ""), header = TRUE)
        
        output <- data.frame(Prediction = predict(model,test), round(predict(model, test, type = 'prob'),3))
        print(output)
        
    })
    
    # Status/Output Text-Box
    output$contents <- renderPrint({
        if(input$submitbutton > 0 ){
            isolate("Calculation Completed.")
        } else{
            return("Ready for Prediction.")
        }
    })
    
    # Prediction results table
    output$tbldata <- renderTable({
        if (input$submitbutton > 0) {
            isolate(DataSetInput())
        }
        
    })
    
   
    
}

    
    
    
    
#     
# shinyServer(function(input, output, session){
# 
#     # input data
#     DataSetInput <- reactive({
#         
#         churn_df <- data.frame(
#             Name = c("ContractRenewal",
#                      "CustServCalls",
#                      "RoamMins"),
#             value = as.character(c(input$ContractRenewal,
#                                    input$CustServCalls,
#                                    input$RoamMins)),
#             stringsAsFactors = F)
#         
#         Churn <- 0
#         churn_df <- rbind(churn_df, Churn)
#         
#         input <- transpose(churn_df)
#         write.table(input, "input.csv", sep = ",", quote = FALSE, row.names = FALSE, col.names = FALSE)
#         
#         test <- read.csv(paste("input",".csv", sep = ""), header = TRUE)
#         
#         output <- data.frame(Prediction = predict(model,test), round(predict(model, test, type = 'prob'),3))
#         print(output)
#         
#     })
#     
#     # Status/Output Text-Box
#     output$contents <- renderPrint({
#         if(input$submitbutton > 0 ){
#             isolate("Calculation Complete.")
#         } else{
#             return("Ready for Prediction.")
#         }
#     })
#     
#     # Prediction results table
#     output$tbldata <- renderTable({
#         if (input$submitbutton>0) {
#             isolate(DataSetInput())
#         }
#     })
# 
# })
# 
