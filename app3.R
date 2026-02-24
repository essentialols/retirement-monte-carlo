library(shiny)

ui <- fluidPage(
  titlePanel("Retirement Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("contrib", "Annual Contribution:", 10000, min = 0),
      numericInput("contrib_years", "Years of Contribution:", 25, min = 1),
      numericInput("return", "Annual Return (%)", 7, min = 0),
      numericInput("retire_years", "Years of Retirement:", 30, min = 1),
      numericInput("simulations", "Number of Simulations:", 1000, min = 100),
      actionButton("run", "Run Simulation")
    ),
    
    mainPanel(
      plotOutput("plot"),
      textOutput("summary")
    )
  )
)

server <- function(input, output) {
  sim_data <- eventReactive(input$run, {
    set.seed(123)  # For reproducibility
    
    final_values <- replicate(input$simulations, {
      balance <- 0
      for (i in 1:input$contrib_years) {
        balance <- balance * (1 + input$return/100) + input$contrib
      }
      for (i in 1:(input$retire_years - 1)) {  # Adjust for payouts
        balance <- balance * (1 + input$return/100) - input$contrib 
      }
      balance
    })
    final_values
  })
  
  output$plot <- renderPlot({
    hist(sim_data(), breaks = 30, main = "Final Retirement Values", xlab = "Value ($)")
  })
  
  output$summary <- renderText({
    paste0("Median Final Value: $", round(median(sim_data()), 2),
           "\nSuccess Rate (>= 0): ", round(mean(sim_data() >= 0) * 100, 1), "%")
  })
}

shinyApp(ui = ui, server = server)
