ui <- fluidPage(pageWithSidebar(
  headerPanel("Visualization"),
  sidebarPanel(
    selectInput("delays",
                h3("Select type of delay"),
                list("Carrier" = "CarrierDelay", 
                     "Weather" = "WeatherDelay", 
                     "NAS" = "NASDelay","Security"="SecurityDelay",
                     "LateAircraft"="LateAircraftDelay"),selected = "CarrierDelay" ),
    selectInput("var",
                h3("Select categorical variable"),
                list("Destination" = "Dest", 
                     "Origin" = "Origin", 
                     "Carrier" = "UniqueCarrier","Airplane"="TailNum",
                     "CancellationCode"="CancellationCode"),selected = "Dest" ),
    radioButtons("plot_cont",
                 h3("Select plot"),
                 list("Histotgram" = 1, 
                      "Scatterplot" = 2,"ViolinPlot"=3),selected = 1 ),
    radioButtons("plot_cat",
                 h3("Select plot"),
                 list("Barplot" = 1, 
                      "Pie Chart" = 2, 
                      "Rose wind" = 3),selected = 1 )),
  mainPanel(tabsetPanel(
    tabPanel("Delays",plotOutput("cont")),
    tabPanel("Categorical",plotOutput("cat")))
  )))

server <- function(input, output) {
  observe({
    if (input$plot_cont == 1){
      output$cont <- renderPlot({
        ggplot(flights, aes(flights[[input$delays]])) +
          geom_histogram(breaks=seq(0, 100, by =2),
                         col="red",
                         aes(fill=..count..)) +
          scale_fill_gradient("Count", low = "green", high = "red") +
          labs(title=cat("Histogram for", input$delays,"time"), x=input$delays,y="# of flights")
      })
    }else if (input$plot_cont == 2 ){
      output$cont <- renderPlot({
        ggplot(flights,
               aes(x=Full_Date,
                   y=flights[[input$delays]],
                   color= UniqueCarrier,alpha =1/3))+
          geom_point()+ theme_bw(base_family='Times')+
          theme(axis.text.x=element_blank(),
                axis.ticks.x=element_blank())
      })
    }
    else{
      output$cont <- renderPlot({
        ggplot(flights, aes(factor(DayOfWeek), flights[[input$delays]]))+ 
          geom_violin(aes(fill = factor(DayOfWeek)),trim = FALSE)+ guides(fill=FALSE)+
          scale_y_continuous(limits = c(0, 25))+
          labs( y=input$delays,x="Day of Week")
      })
    }
    
    if (input$plot_cat == 1 ){
      output$cat <- renderPlot({
        ggplot (flights)+ aes (as.factor(flights[[input$var]]), fill = as.factor(Cancelled)) + 
          geom_bar()
      })
    }else if (input$plot_cat == 2 ){
      output$cat <- renderPlot({
        ggplot(flights, aes(x = factor(1), fill = as.factor(flights[[input$var]]))) +
          geom_bar(width = 1) + coord_polar(theta = "y")
      })
    }else {
      output$cat <- renderPlot({
        ggplot(flights, aes(x = DayOfWeek, fill = input$var)) + geom_bar(width = 1) + coord_polar()
      })
    }
  })
}
shinyApp(ui = ui, server = server)