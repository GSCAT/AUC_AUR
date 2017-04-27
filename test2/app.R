#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# git test

library(shiny)
library(formattable)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("AUC AUR"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         # sliderInput("bins",
         #             "Number of bins:",
         #             min = 1,
         #             max = 50,
         #             value = 30),
         uiOutput("select_variable")
         #,submitButton("Update View")
         # checkboxGroupInput("Variable", "Variable:", names(PCF_post_proc[13:21]), selected = c("Fiscal Quarter", "Market"))
         # checkboxGroupInput("Variable", "Variable:", choices = c(`Fiscal Month #`, `Fiscal Quarter`, `Brand`, `Market`), selected = c("Fiscal Quarter", "Market"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         dataTableOutput("dt", width = 660)
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
# fold ----   
   # output$distPlot <- renderPlot({
   #    # generate bins based on input$bins from ui.R
   #    x    <- faithful[, 2] 
   #    bins <- seq(min(x), max(x), length.out = input$bins + 1)
   #    
   #    # draw the histogram with the specified number of bins
   #    hist(x, breaks = bins, col = 'darkgray', border = 'white')
   # }),
  output$select_variable <- renderUI({ 
    checkboxGroupInput("variable", "Variable:", names(c(PCF_post_proc[13:21])), 
                       selected = c("Fiscal Quarter", "Market"))

  })
    # Output PCF ----
 output$dt <-  renderDataTable({
   
   #input_vec <- as.vector(input$variable)
   # input_vec <- c(as.name(as.vector(input$variable)))
   input_vec <- lapply(input$variable, as.name)
   print(input_vec)
   
   Output_PCF_test <- PCF_post_proc %>% 
    # group_by(`Fiscal Quarter`, `Channel`) %>% 
    # group_by_(.dots = as.vector(input_vec)) %>% 
    group_by_(.dots = input_vec) %>% 
    summarise("Forecast TY AUR of Sales" = sum(subset(`Retail$`, Source == "Forecast"), na.rm = TRUE)/sum(subset(`Unit Sales`, Source == "Forecast"), na.rm = TRUE),
              "TY AUC of Receipts" = sum(subset(`Cost Rcpts`, Source == "Forecast"), na.rm = TRUE)/sum(subset(`Unit Rcpts`, Source == "Forecast"), na.rm = TRUE),
              "Budget AUR of Sales"= sum(subset(`Retail$`, Source == "Budget"),na.rm = TRUE)/sum(subset(`Unit Sales`, Source == "Budget"), na.rm = TRUE),
              "Budget AUC of Receipts" = sum(subset(`Cost Rcpts`, Source == "Budget"), na.rm = TRUE)/sum(subset(`Unit Rcpts`, Source == "Budget"), na.rm = TRUE),
              "LY AUR of Sales" = sum(subset(`Retail$`, Source == "LY"), na.rm = TRUE)/sum(subset(`Unit Sales`, Source == "LY"), na.rm = TRUE),
              "LY AUC of Receipts" = sum(subset(`Cost Rcpts`, Source == "LY"), na.rm = TRUE)/sum(subset(`Unit Rcpts`, Source == "LY"), na.rm = TRUE),
              "AUR % Change (TY vs Budget)" = as.numeric((`Forecast TY AUR of Sales`-`Budget AUR of Sales`)/`Budget AUR of Sales`)*100,
              "AUC % Change (TY vs Budget)" = as.numeric((`TY AUC of Receipts`-`Budget AUC of Receipts`)/`Budget AUC of Receipts`)*100,
              "AUR % Change (TY vs LY)" = as.numeric((`Forecast TY AUR of Sales`-`LY AUR of Sales`)/`LY AUR of Sales`)*100,
              "AUC % Change (TY vs LY)" = as.numeric((`TY AUC of Receipts`-`LY AUC of Receipts`)/`LY AUC of Receipts`)*100,
              "GM Budget" = sum((subset(`Retail$`, Source == "Budget") - subset(`Cost$`, Source == "Budget")), na.rm = TRUE)/ sum(subset(`Retail$`, Source == "Budget"), na.rm = TRUE)*100,
              "GM Forecast/Actual" = sum((subset(`Retail$`, Source == "Forecast") - subset(`Cost$`, Source == "Forecast")), na.rm = TRUE)/ sum(subset(`Retail$`, Source == "Forecast"), na.rm = TRUE)*100,
              "GM LY" = sum((subset(`Retail$`, Source == "LY") - subset(`Cost$`, Source == "LY")), na.rm = TRUE)/ sum(subset(`Retail$`, Source == "LY"), na.rm = TRUE)*100,
              "GM Budget (Dollars)" = sum((subset(`Retail$`, Source == "Budget") - subset(`Cost$`, Source == "Budget")), na.rm = TRUE),
              "GM Forecast/Actual (Dollars)" = sum((subset(`Retail$`, Source == "Forecast") - subset(`Cost$`, Source == "Forecast")), na.rm = TRUE),
              "GM LY (Dollars)" = sum((subset(`Retail$`, Source == "LY") - subset(`Cost$`, Source == "LY")), na.rm = TRUE))
  datatable(Output_PCF_test)
  })
 # print(Variable)
}

# Run the application 
shinyApp(ui = ui, server = server)

