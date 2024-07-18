library(shiny)
library(ggplot2)
library(scales)
library(Rcpp)

sourceCpp("portfolio_simulation.cpp")

# Define UI for application
ui <- fluidPage(
  titlePanel("Monte Carlo Simulation for Stock Investments"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("meanReturn", "Mean return (%)", value = 6),
      numericInput("sdReturn", "Standard deviation of return (%)", value = 20),
      selectInput("timeUnit", "Time unit:", choices = c("Months", "Years")),
      numericInput("meanInflation", "Mean inflation (%)", value = 2.5),
      numericInput("sdInflation", "Standard deviation of inflation (%)", value = 1),
      numericInput("numSimulations", "Number of simulations", value = 5000),
      numericInput("startBalance", "Starting balance ($)", value = 200*10^3),
      numericInput("monthlyContribution", "Monthly contribution ($)", value = 500),
      numericInput("annualIncrease", "Annual contribution increase (%)", value = 2),
      numericInput("contributionYears", "Years of Contributions", value = 15),
      numericInput("retirementYears", "Years of Retirement", value = 50),
      numericInput("withdrawalRate", "Annual withdrawal rate (%) during retirement", value = 4),
      actionButton("runSim", "Run Simulation")
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Balance at Retirement", plotOutput("plot1")),
                  tabPanel("Balance at End of Retirement", plotOutput("plot2")),
                  tabPanel("Balance Over Time", plotOutput("balancePlot")),
                  tabPanel("Withdrawals Over Time", plotOutput("withdrawalPlot"))
      ),
      verbatimTextOutput("statsOutput")
    )
  )
)


# Define server logic for simulation
server <- function(input, output) {
    # results <- eventReactive(input$runSim, {
    #   # set.seed(123)
    #   totalMonths <- (input$contributionYears + input$retirementYears) * 12
    #   contributionMonths <- input$contributionYears * 12
    #   totalMonths <- (input$contributionYears + input$retirementYears) * 12  # New calculation
    #   replicate(input$numSimulations, {
    #     monthlyReturns <- rnorm(totalMonths, mean = input$meanReturn / 100 / 12, sd = input$sdReturn / 100 / 12)
    #     monthlyInflation <- rnorm(totalMonths, mean = input$meanInflation / 100 / 12, sd = input$sdInflation / 100 / 12)
    #     balance <- numeric(totalMonths)
    #     withdrawals <- numeric(totalMonths)
    #     balance[1] <- input$startBalance
    #     currentContribution <- input$monthlyContribution
    #     annualIncreaseFactor <- 1 + input$annualIncrease / 100
    #     monthlyWithdrawalRate <- input$withdrawalRate / 100 / 12
    #     
    #     for (i in 2:totalMonths) {
    #       contribution <- ifelse(i <= contributionMonths, currentContribution, 0)
    #       withdrawal <- ifelse(i > contributionMonths, balance[i-1] * monthlyWithdrawalRate, 0)
    #       withdrawals[i] <- withdrawal
    #       if ((i-1) %% 12 == 0 && i <= contributionMonths) {
    #         currentContribution <- currentContribution * annualIncreaseFactor
    #       }
    #       contributionAdjustedForInflation <- contribution * (1 + monthlyInflation[i-1])
    #       balance[i] <- balance[i-1] * (1 + monthlyReturns[i-1]) + contributionAdjustedForInflation - withdrawal
    #     }
    #     list(
    #       balanceAtRetirement = balance[contributionMonths], 
    #       balanceAtEnd = balance[totalMonths],
    #       meanWithdrawal = mean(withdrawals[contributionMonths + 1:totalMonths], na.rm = TRUE),
    #       balance = balance,
    #       withdrawals = withdrawals,
    #       totalMonths = totalMonths,
    #       contributionMonths
    #     )
    #   }, simplify = FALSE)
    # })
    
    results <- eventReactive(input$runSim, {
      # set.seed(123)
      totalMonths <- (input$contributionYears + input$retirementYears) * 12
      contributionMonths <- input$contributionYears * 12
      
      # Call the C++ function
      simulatePortfolio(input$numSimulations, input$startBalance, input$meanReturn, input$sdReturn,
                        input$meanInflation, input$sdInflation, totalMonths, contributionMonths,
                        input$monthlyContribution, input$annualIncrease, input$withdrawalRate)
    })
  
  # Plot outputs and statsOutput remain similar but now include meanWithdrawal calculations
  output$plot1 <- renderPlot({
    data <- results()
    balancesAtRetirement <- sapply(data, function(x) x$balanceAtRetirement)
    
    meanValue <- mean(balancesAtRetirement)
    medianValue <- median(balancesAtRetirement)
    lowerCI <- quantile(balancesAtRetirement, 0.025)
    upperCI <- quantile(balancesAtRetirement, 0.975)
    
    ggplot(data.frame(FinalBalanceAtRetirement = balancesAtRetirement), aes(x = FinalBalanceAtRetirement)) +
      geom_histogram(bins = 50, fill = 'blue', color = 'black') +
      labs(title = "Balance at Retirement Age", x = "Final Balance ($)", y = "Frequency") +
      theme_minimal() +
      scale_x_continuous(labels = scales::dollar_format()) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_vline(xintercept = meanValue, color = "red", linetype = "dashed", size = 1) +
      geom_vline(xintercept = medianValue, color = "green", linetype = "dashed", size = 1) +
      geom_vline(xintercept = lowerCI, color = "blue", linetype = "dotted", size = 1) +
      geom_vline(xintercept = upperCI, color = "blue", linetype = "dotted", size = 1)
  })
  
  output$plot2 <- renderPlot({
    data <- results()
    balancesAtEnd <- sapply(data, function(x) x$balanceAtEnd)
    
    meanValue <- mean(balancesAtEnd)
    medianValue <- median(balancesAtEnd)
    lowerCI <- quantile(balancesAtEnd, 0.025)
    upperCI <- quantile(balancesAtEnd, 0.975)
    
    ggplot(data.frame(FinalBalanceAtEnd = balancesAtEnd), aes(x = FinalBalanceAtEnd)) +
      geom_histogram(bins = 50, fill = 'blue', color = 'black') +
      labs(title = "Balance at End of Retirement Period", x = "Final Balance ($)", y = "Frequency") +
      theme_minimal() +
      scale_x_continuous(labels = scales::dollar_format()) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_vline(xintercept = meanValue, color = "red", linetype = "dashed", size = 1) +
      geom_vline(xintercept = medianValue, color = "green", linetype = "dashed", size = 1) +
      geom_vline(xintercept = lowerCI, color = "blue", linetype = "dotted", size = 1) +
      geom_vline(xintercept = upperCI, color = "blue", linetype = "dotted", size = 1)
  })
  
  # Plot for Balance Over Time
  output$balancePlot <- renderPlot({
    simulationResults <- results()
    totalMonths <- simulationResults[[1]]$totalMonths  # Assuming all simulations have the same length
    
    # Gather all simulation balances into a single data frame for plotting
    allBalances <- do.call(cbind, lapply(simulationResults, function(x) x$balance))
    
    # Prepare data frame for ggplot
    df <- data.frame(
      Month = rep(1:totalMonths, times = ncol(allBalances)),
      Balance = as.vector(allBalances),
      Simulation = factor(rep(1:ncol(allBalances), each = totalMonths))
    )
    
    # Plotting
    ggplot(df, aes(x = Month, y = Balance)) +
      geom_line(aes(group = Simulation), color = "grey", alpha = 0.5) +  # Plot each simulation in light grey
      geom_line(data = data.frame(Month = 1:totalMonths, Balance = rowMeans(allBalances)), aes(y = Balance), color = "blue") +  # Add the mean balance line in blue
      labs(title = "Average Balance Over Time", x = "Month", y = "Average Balance ($)") +
      theme_minimal() +
      scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "", big.mark = ",", decimal.mark = ".", accuracy = 1))  # Customize the dollar format as needed
  })
  
  
  
  # Plot for Withdrawals Over Time
  output$withdrawalPlot <- renderPlot({
    simulationResults <- results()
    totalMonths <- simulationResults[[1]]$totalMonths  # Assuming all simulations have the same length
    
    # Gather all simulation withdrawals into a single data frame for plotting
    allWithdrawals <- do.call(cbind, lapply(simulationResults, function(x) x$withdrawals))
    
    # Prepare data frame for ggplot
    df <- data.frame(
      Month = rep(1:totalMonths, times = ncol(allWithdrawals)),
      Withdrawal = as.vector(allWithdrawals),
      Simulation = factor(rep(1:ncol(allWithdrawals), each = totalMonths))
    )
    
    
    # Plotting
    ggplot(df, aes(x = Month, y = Withdrawal)) +
      geom_line(aes(group = Simulation), color = "grey", alpha = 0.5) +  # Plot each simulation in light grey
      geom_line(data = data.frame(Month = 1:totalMonths, Withdrawal = rowMeans(allWithdrawals)), 
                aes(y = Withdrawal), color = "red") +  # Add the mean withdrawal line in red
      labs(title = "Average Withdrawals Over Time", x = "Month", y = "Average Withdrawal ($)") +
      theme_minimal() +
      scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "", big.mark = ",", decimal.mark = ".", accuracy = 1))  # Customize the dollar format as needed
  })
  
  
  
  output$statsOutput <- renderPrint({
    data <- results()
    balancesAtRetirement <- sapply(data, function(x) x$balanceAtRetirement)
    balancesAtEnd <- sapply(data, function(x) x$balanceAtEnd)
    meanWithdrawal <- sapply(data, function(x) x$meanWithdrawal)
    
    cat("Withdrawal Amount during Retirement Period:\n")
    cat("Mean: ", dollar_format()(mean(meanWithdrawal)), "\n")
    cat("Median: ", dollar_format()(median(meanWithdrawal)), "\n")
    cat("95% Confidence Interval: [",
        dollar_format()(quantile(meanWithdrawal, 0.025)), ",",
        dollar_format()(quantile(meanWithdrawal, 0.975)), "]\n\n")
    
    cat("Statistics at Retirement Age:\n")
    cat("Mean: ", dollar_format()(mean(balancesAtRetirement)), "\n")
    cat("Median: ", dollar_format()(median(balancesAtRetirement)), "\n")
    cat("95% Confidence Interval: [",
        dollar_format()(quantile(balancesAtRetirement, 0.025)), ",",
        dollar_format()(quantile(balancesAtRetirement, 0.975)), "]\n\n")
    
    cat("Statistics at End of Retirement Period:\n")
    cat("Mean: ", dollar_format()(mean(balancesAtEnd)), "\n")
    cat("Median: ", dollar_format()(median(balancesAtEnd)), "\n")
    cat("95% Confidence Interval: [",
        dollar_format()(quantile(balancesAtEnd, 0.025)), ",",
        dollar_format()(quantile(balancesAtEnd, 0.975)), "]\n")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
