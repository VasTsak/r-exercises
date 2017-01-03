ui <- fluidPage(pageWithSidebar(
  headerPanel("Descriptive Analysis"),
  sidebarPanel(
    selectInput("fun",
                h3("Select functions"),
                list("Summary" = 1, 
                     "Structure" = 2, 
                     "Head" = 3,
                     "Tail"=4,"Names"=5),selected = 1 ),
    selectInput("me",
              h3("Select measure"),
              list("Mean" = 1, 
                   "Median" = 2, 
                   "Max" = 3,
                   "Min"=4,"Range"=5,"Standard  Deviation"=6),selected = 1 ),
    selectInput("var",
              h3("Select variable"),
              list("ActualElapsedTime" = "ActualElapsedTime", 
                   "CRSElapsedTime" = "CRSElapsedTime", 
                   "AirTime" = "AirTime",
                   "ArrDelay"="ArrDelay","DepDelay"="DepDelay","TaxiIn"="TaxiIn","TaxiOut"="TaxiOut"),selected = "ActualElapsedTime" )),
   mainPanel(tabsetPanel(
    tabPanel("Content",verbatimTextOutput("cont")),
    tabPanel("Measures",verbatimTextOutput("meas"))))
  ))
server <- function(input, output) {
  output$cont <- renderPrint({
    if (input$fun == 1 ){
      print(summary(flights))
    }
    else if (input$fun == 2 ){
      print(str(flights))
    }
    else if (input$fun == 3 ){
      print(head(flights))
    }
    else if (input$fun == 4 ){
      print(tail(flights))
    }
    else {
      print(names(flights))
    }
  })
  output$meas <- renderPrint({
    if (input$me == 1 ){
      print(mean(flights[[input$var]],na.rm = TRUE))
    }
    else if (input$me == 2 ){
      print(median(flights[[input$var]],na.rm = TRUE))
    }
    else if (input$me == 3 ){
      print(max(flights[[input$var]],na.rm = TRUE))
    }
    else if (input$me == 4 ){
      print(min(flights[[input$var]],na.rm = TRUE))
    }
    else if (input$me == 5 ){
      print(range(flights[[input$var]],na.rm = TRUE))
    }
    else{
      print(sd(flights[[input$var]],na.rm = TRUE))
    }
  })
  
  
}
shinyApp(ui = ui, server = server)