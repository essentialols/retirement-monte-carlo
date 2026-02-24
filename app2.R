library(shiny)
library(ggplot2)
library(scales)
library(Rcpp)
library(shinyjs)  # Add this line

library(shinythemes)

sourceCpp("portfolio_simulation2.cpp")

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("flatly"),  # Choose a theme (e.g., "flatly")
  useShinyjs(),
  
  titlePanel("Monte Carlo Simulation for Stock Investments"),
  
  sidebarLayout(
    sidebarPanel(
      tags$style(HTML("
        .shiny-input-container { font-size: 12px; }
        .input-group { margin-bottom: 10px; }
        
        /* Color Categories */
        .group1 .shiny-input-container { background-color: #f0f0f5; } /* Light Blue */
        .group2 .shiny-input-container { background-color: #f5f5f0; } /* Light Yellow */
        .group3 .shiny-input-container { background-color: #f0fff0; } /* Light Green */
        .group4 .shiny-input-container { background-color: #fff5ee; } /* Light Orange */
      ")),
      
      # --- Investment Returns & Inflation (Group 1) ---
      div(class = "group1",
          numericInput("meanReturn", "Mean return (%)", value = 6),
          numericInput("sdReturn", "Standard deviation of return (%)", value = 20),
          numericInput("meanInflation", "Mean inflation (%)", value = 3.28),
          numericInput("sdInflation", "Standard deviation of inflation (%)", value = 4.93)
      ),
      
      # --- Contributions (Group 2) ---
      div(class = "group2",
          numericInput("startBalance", "Starting balance ($)", value = 200*10^3),
          numericInput("monthlyContribution", "Monthly contribution ($)", value = 2500),
          numericInput("annualIncrease", "Annual contribution increase (%)", value = 5),
          numericInput("contributionYears", "Years of Contributions", value = 15)
      ),
      
      # --- Retirement (Group 3) ---
      div(class = "group3",
          numericInput("retirementYears", "Years of Retirement", value = 50),
          radioButtons("withdrawalType", "Withdrawal Type:",
                       choices = list("Fixed Dollar Amount" = "fixed", 
                                      "Percentage of Balance" = "percent"),
                       selected = "fixed"),
          conditionalPanel(
            condition = "input.withdrawalType == 'fixed'",
            numericInput("fixedWithdrawal", "Fixed monthly pre-tax withdrawal ($)", value = 4000, min = 0)
          ),
          conditionalPanel(
            condition = "input.withdrawalType == 'percent'",
            selectInput("timeUnit", "Time unit:", choices = c("Months", "Years")),
            numericInput("withdrawalRate", "Annual/Monthly pre-tax withdrawal rate (%) during retirement", value = 4)
          )
      ),
      
      # --- Simulation (Group 4) ---
      div(class = "group4",
          numericInput("numSimulations", "Number of simulations", value = 500),
          actionButton("runSim", "Run Simulation")
      )
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Balance at Retirement", plotOutput("plot1")),
                  tabPanel("Balance at End of Retirement", plotOutput("plot2")),
                  tabPanel("Balance Over Time", plotOutput("balancePlot")),
                  tabPanel("Withdrawals Over Time", plotOutput("withdrawalPlot")),
                  tabPanel("Monthly Contributions Table", tableOutput("contribTable"))
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
                      input$monthlyContribution, input$annualIncrease, input$withdrawalType == 'percent',
                      input$withdrawalRate, input$fixedWithdrawal)
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
    allBalances <- do.call(cbind, lapply(simulationResults, function(x) x$balances))
    
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
    totalMonths <- simulationResults[[1]]$totalMonths
    
    # Gather all simulation withdrawals into a single data frame for plotting
    allWithdrawals <- do.call(cbind, lapply(simulationResults, function(x) x$withdrawals))
    
    # Prepare data frame for ggplot
    df <- data.frame(
      Month = rep(1:totalMonths, times = ncol(allWithdrawals)),
      Withdrawal = as.vector(allWithdrawals),
      Simulation = factor(rep(1:ncol(allWithdrawals), each = totalMonths))
    )
    
    # Plotting with mean line and customization
    ggplot(df, aes(x = Month, y = Withdrawal)) +
      # geom_line(aes(group = Simulation), color = "grey", alpha = 0.5) +
      geom_line(data = data.frame(Month = 1:totalMonths, Withdrawal = rowMeans(allWithdrawals)), aes(y = Withdrawal), color = "red") +
      labs(title = "Average Withdrawals Over Time", x = "Month", y = "Average Withdrawal ($)") +
      theme_minimal() +
      scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "", big.mark = ",", decimal.mark = ".", accuracy = 1))  # Customize the dollar format as needed
  })
  
  
  
  
  
  # Output the results as a table, only updates when 'runSim' is clicked
  output$contribTable <- renderTable({
    if(is.null(results())) return()  # Ensure this only runs after 'runSim' is pressed
    
    # Calculate contributions for each year
    contribution_years <- seq_len(input$contributionYears)
    annual_increase_factor <- 1 + input$annualIncrease / 100
    inflation_factor <- 1 + input$meanInflation / 100
    contributions_nominal <- numeric(length = input$contributionYears)
    contributions_inflation_adjusted <- numeric(length = input$contributionYears)
    
    contributions_nominal[1] <- input$monthlyContribution
    contributions_inflation_adjusted[1] <- input$monthlyContribution  # Initial contribution adjusted for inflation
    
    for (i in 2:length(contributions_nominal)) {
      contributions_nominal[i] <- contributions_nominal[i - 1] * annual_increase_factor
      contributions_inflation_adjusted[i] <- contributions_nominal[i] / (inflation_factor ^ i)
    }
    
    # Create a data frame for display
    data.frame(
      Year = contribution_years,
      NominalContribution = contributions_nominal,
      InflationAdjustedContribution = contributions_inflation_adjusted
    )
  })
  
  
  
  output$statsOutput <- renderPrint({
    data <- results()
    # balancesAtRetirement <- sapply(data, function(x) x$balanceAtRetirement)
    # balancesAtEnd <- sapply(data, function(x) x$balanceAtEnd)
    # meanWithdrawal <- sapply(data, function(x) x$meanWithdrawal)
    # 
    # meanWithdrawalDisplayed <- sapply(data, function(x) x$meanWithdrawalDisplayed)
    
    cat("Withdrawal Amount during Retirement Period (in Today's Dollars):\n")
    cat("Mean: ", dollar_format()(mean(meanWithdrawalDisplayed)), "\n")
    cat("Median: ", dollar_format()(median(meanWithdrawalDisplayed)), "\n")
    cat("95% Confidence Interval: [",
        dollar_format()(quantile(meanWithdrawalDisplayed, 0.025)), ",",
        dollar_format()(quantile(meanWithdrawalDisplayed, 0.975)), "]\n\n")
    
    cat("Statistics at Retirement Age:\n")
    cat("Mean: ", dollar_format()(mean(balancesAtRetirement)), "\n")
    cat("Median: ", dollar_format()(median(balancesAtRetirement)), "\n")
    cat("95% Confidence Interval: [",
        dollar_format()(quantile(balancesAtRetirement, 0.025)), ",",
        dollar_format()(quantile(balancesAtRetirement, 0.975)), "]\n")
    cat("90% Confidence Interval: [",
        dollar_format()(quantile(balancesAtRetirement, 0.05)), ",",
        dollar_format()(quantile(balancesAtRetirement, 0.95)), "]\n\n")
    
    cat("Statistics at End of Retirement Period:\n")
    cat("Mean: ", dollar_format()(mean(balancesAtEnd)), "\n")
    cat("Median: ", dollar_format()(median(balancesAtEnd)), "\n")
    cat("95% Confidence Interval: [",
        dollar_format()(quantile(balancesAtEnd, 0.025)), ",",
        dollar_format()(quantile(balancesAtEnd, 0.975)), "]\n")
    cat("90% Confidence Interval: [",
        dollar_format()(quantile(balancesAtEnd, 0.05)), ",",
        dollar_format()(quantile(balancesAtEnd, 0.95)), "]\n")
    
    # Probability of balance at end being below $0
    probBelowZero <- mean(balancesAtEnd < 0)
    cat("Probability of negative balance at end of retirement: ", 
        formatC(probBelowZero * 100, format = "f", digits = 2), "%\n")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
