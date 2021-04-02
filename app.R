#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Bond Calculator"),
   
   # Input values
   sidebarLayout(
      sidebarPanel(
        numericInput("interest","Interest Rate:",min = 0, max = 1,step = 0.01, value = 0.1),
        numericInput("principal","Principal Amount:",min = 0, max = 1000000000,step = 0.01, value = 1000),
        numericInput("coupon","Coupon Rate:",min = 0, max = 1,step = 0.01, value = 0.05),
        numericInput("year","Years:",min = 0, max = 100,step = 0.01, value = 10),
        radioButtons("rate","Payment Frequency",c("Annual" = "a", "Semi-Annual" = "s", "Quarterly" = "q", "Monthly"="m")),
        actionButton("do","Calculate")
      ),
      
      # Show the calculated price
      mainPanel(
         textOutput("price")
      )
   )
)

# Define server logic
server <- function(input, output) {
  #Calculates price if you click calculate
  formula <- eventReactive(input$do,{
    rate <- switch(input$rate,
                   a = 1,
                   s = 2,
                   q = 4,
                   m = 12)
    principal <- input$principal
    interest <- input$interest/rate
    year <- input$year*rate
    coupon <- input$coupon/rate
    #price <- principal/(1+interest)^year
    #for (i in 1:year){
    #   price <- price + (coupon*principal)/(1+interest)^i
    #}
    #price
    i <- seq(1,year)
    sum((coupon*principal)/(1+interest)^i,principal/(1+interest)^year)
  })
  
  #Output text
  output$price <- renderText({
    paste("The price is",formula())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

