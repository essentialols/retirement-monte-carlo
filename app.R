# Import libraries
library(shiny)
library(ggplot2)
library(scales)
library(Rcpp)
library(shinyjs)

sourceCpp("simulatePortfolio.cpp")

# Define UI function
ui <- function() {
  fluidPage(
    useShinyjs(),
    titlePanel("Monte Carlo Simulation for Stock Investments"),
    sidebarLayout(
      sidebarPanel(
        tags$style(HTML("
          .shiny-input-container { font-size: 12px; }
        ")),
        numericInput("mean_return", "Mean return (%)", value = 6),
        numericInput("sd_return", "Standard deviation of return (%)", value = 20),
        numericInput("mean_inflation", "Mean inflation (%)", value = 3.28),
        numericInput("sd_inflation", "Standard deviation of inflation (%)", value = 4.93),
        numericInput("start_balance", "Starting balance ($)", value = 200*10^3),
        numericInput("monthly_contribution", "Monthly contribution ($)", value = 2500),
        numericInput("annual_increase", "Annual contribution increase (%)", value = 5),
        numericInput("contribution_years", "Years of Contributions", value = 15),
        numericInput("retirement_years", "Years of Retirement", value = 50),
        radioButtons("withdrawal_type", "Withdrawal Type:",
                     choices = list("Fixed Dollar Amount" = "fixed", "Percentage of Balance" = "percent"),
                     selected = "fixed"),
        numericInput("fixed_withdrawal", "Fixed monthly pre-tax withdrawal ($)", value = 4000, min = 0),
        selectInput("time_unit", "Time unit:", choices = c("Months", "Years")),
        numericInput("withdrawal_rate", "Annual pre-tax withdrawal rate (%) during retirement", value = 4),
        numericInput("num_simulations", "Number of simulations", value = 500),
        actionButton("run_sim", "Run Simulation")
      ),
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Balance at Retirement", plotOutput("retirement_balance_histogram")),
                    tabPanel("Balance at End of Retirement", plotOutput("end_retirement_balance_histogram")),
                    tabPanel("Balance Over Time", plotOutput("balance_over_time_plot")),
                    tabPanel("Withdrawals Over Time", plotOutput("withdrawals_over_time_plot")),
                    tabPanel("Monthly Contributions Table", tableOutput("contrib_table")),
                    tabPanel("Withdrawal Table", 
                             tableOutput("withdrawal_table"),
                             textOutput("mean_inflation_adjusted_withdrawal")  # Added this line to display the mean inflation-adjusted withdrawal
                    ),
                    tabPanel("Future Nominal Withdrawals", 
                             tableOutput("future_nominal_table"),
                             textOutput("future_nominal_mean")
                    )
        ),
        verbatimTextOutput("stats_output")
      )
    )
  )
}

# Define server function
simulate_portfolio <- function(num_simulations, start_balance, mean_return, sd_return,
                               mean_inflation, sd_inflation, total_months, contribution_months,
                               monthly_contribution, annual_increase, withdrawal_type_percent,
                               withdrawal_rate, fixed_withdrawal) {
  # Call the C++ function
  results <- simulatePortfolio(num_simulations, start_balance, mean_return, sd_return,
                               mean_inflation, sd_inflation, total_months, contribution_months,
                               monthly_contribution, annual_increase, withdrawal_type_percent,
                               withdrawal_rate, fixed_withdrawal)
  
  # Convert the results to a list of lists
  parsed_results <- lapply(results, function(x) {
    list(
      balance_at_retirement = x$balanceAtRetirement,
      balance_at_end = x$balanceAtEnd,
      mean_withdrawal_displayed = x$meanWithdrawalDisplayed,
      balances = x$balances,
      withdrawals = x$withdrawals,
      total_months = x$totalMonths,
      contribution_months = x$contributionMonths
    )
  })
  
  return(parsed_results)
}

server <- function(input, output) {
  # Reactive function to run simulation
  results <- eventReactive(input$run_sim, {
    # Input validation
    validate(
      need(input$mean_return >= 0, "Mean return must be non-negative."),
      need(input$sd_return >= 0, "Standard deviation of return must be non-negative."),
      need(input$mean_inflation >= 0, "Mean inflation must be non-negative."),
      need(input$sd_inflation >= 0, "Standard deviation of inflation must be non-negative."),
      need(input$start_balance >= 0, "Starting balance must be non-negative."),
      need(input$monthly_contribution >= 0, "Monthly contribution must be non-negative."),
      need(input$annual_increase >= 0, "Annual contribution increase must be non-negative."),
      need(input$contribution_years > 0, "Years of Contributions must be positive."),
      need(input$retirement_years > 0, "Years of Retirement must be positive."),
      need(input$fixed_withdrawal >= 0, "Fixed monthly pre-tax withdrawal must be non-negative."),
      need(input$withdrawal_rate >= 0, "Annual pre-tax withdrawal rate must be non-negative."),
      need(input$num_simulations > 0, "Number of simulations must be positive.")
    )
    
    total_months <- (input$contribution_years + input$retirement_years) * 12
    contribution_months <- input$contribution_years * 12
    
    simulate_portfolio(input$num_simulations, input$start_balance, input$mean_return, input$sd_return,
                       input$mean_inflation, input$sd_inflation, total_months, contribution_months,
                       input$monthly_contribution, input$annual_increase, input$withdrawal_type == 'percent',
                       input$withdrawal_rate, input$fixed_withdrawal)
  })
  
  # Plot outputs
  output$retirement_balance_histogram <- renderPlot({
    data <- results()
    balances_at_retirement <- sapply(data, function(x) x$balance_at_retirement)
    plot_histogram(balances_at_retirement, "Balance at Retirement Age")
  })
  
  output$end_retirement_balance_histogram <- renderPlot({
    data <- results()
    balances_at_end <- sapply(data, function(x) x$balance_at_end)
    plot_histogram(balances_at_end, "Balance at End of Retirement Period")
  })
  
  output$balance_over_time_plot <- renderPlot({
    data <- results()
    all_balances <- get_all_balances(data)
    plot_balance_over_time(all_balances)
  })
  
  output$withdrawals_over_time_plot <- renderPlot({
    data <- results()
    all_withdrawals <- get_all_withdrawals(data)
    plot_withdrawals_over_time(all_withdrawals)
  })
  
  output$contrib_table <- renderTable({
    data <- results()
    contributions <- calculate_contributions(input$contribution_years, input$monthly_contribution,
                                             input$annual_increase, input$mean_inflation)
    create_contrib_table(contributions)
  })
  
  output$stats_output <- renderPrint({
    data <- results()
    balances_at_retirement <- sapply(data, function(x) x$balance_at_retirement)
    balances_at_end <- sapply(data, function(x) x$balance_at_end)
    mean_withdrawal_displayed <- sapply(data, function(x) x$mean_withdrawal_displayed)
    
    print_withdrawal_stats(mean_withdrawal_displayed)
    print_balance_stats(balances_at_retirement, "Retirement Age")
    print_balance_stats(balances_at_end, "End of Retirement Period")
    
    prob_below_zero <- mean(balances_at_end < 0)
    cat("Probability of negative balance at end of retirement: ",
        formatC(prob_below_zero * 100, format = "f", digits = 2), "%\n")
  })
  
  output$withdrawal_table <- renderTable({
    req(input$fixed_withdrawal, input$mean_inflation, input$contribution_years, input$retirement_years)
    
    withdrawal_data <- generate_withdrawal_table(
      input$fixed_withdrawal, 
      input$mean_inflation, 
      input$contribution_years,
      input$retirement_years
    )
    
    withdrawal_data$table_data$Nominal_Withdrawal <- scales::dollar(withdrawal_data$table_data$Nominal_Withdrawal)
    withdrawal_data$table_data$Inflation_Adjusted_Withdrawal <- scales::dollar(withdrawal_data$table_data$Inflation_Adjusted_Withdrawal)
    
    withdrawal_data$table_data
  }, digits = 0)
  
  output$mean_inflation_adjusted_withdrawal <- renderText({
    req(input$fixed_withdrawal, input$mean_inflation, input$contribution_years, input$retirement_years)
    
    withdrawal_data <- generate_withdrawal_table(
      input$fixed_withdrawal, 
      input$mean_inflation, 
      input$contribution_years,
      input$retirement_years
    )
    
    paste("Mean Inflation Adjusted Withdrawal: ", scales::dollar(mean(withdrawal_data$table_data$Inflation_Adjusted_Withdrawal)))
  })
  
  
  output$future_nominal_table <- renderTable({
    req(input$fixed_withdrawal, input$mean_inflation, input$contribution_years, input$retirement_years)
    
    result <- generate_future_nominal_table(
      input$fixed_withdrawal, 
      input$mean_inflation, 
      input$contribution_years,
      input$retirement_years
    )
    
    result$table_data$Today_Dollars <- scales::dollar(result$table_data$Today_Dollars)
    result$table_data$Future_Nominal_Dollars <- scales::dollar(result$table_data$Future_Nominal_Dollars)
    
    result$table_data
  }, digits = 0)
  
  output$future_nominal_mean <- renderText({
    req(input$fixed_withdrawal, input$mean_inflation, input$contribution_years, input$retirement_years)
    
    result <- generate_future_nominal_table(
      input$fixed_withdrawal, 
      input$mean_inflation, 
      input$contribution_years,
      input$retirement_years
    )
    
    paste("Mean Future Nominal Withdrawal: ", scales::dollar(result$mean_future_nominal))
  })
}

# Helper functions
plot_histogram <- function(data, title) {
  mean_value <- mean(data)
  median_value <- median(data)
  lower_CI <- quantile(data, 0.025)
  upper_CI <- quantile(data, 0.975)
  
  ggplot(data.frame(FinalBalance = data), aes(x = FinalBalance)) +
    geom_histogram(bins = 50, fill = 'blue', color = 'black') +
    labs(title = title, x = "Final Balance ($)", y = "Frequency") +
    theme_minimal() +
    scale_x_continuous(labels = scales::dollar_format()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_vline(xintercept = mean_value, color = "red", linetype = "dashed", size = 1) +
    geom_vline(xintercept = median_value, color = "green", linetype = "dashed", size = 1) +
    geom_vline(xintercept = lower_CI, color = "blue", linetype = "dotted", size = 1) +
    geom_vline(xintercept = upper_CI, color = "blue", linetype = "dotted", size = 1)
}

plot_balance_over_time <- function(data) {
  if (nrow(data) == 0 || ncol(data) < 3) {
    stop("Data is empty or not correctly structured.")
  }
  
  # Calculate the average balance for each month
  avg_balance <- aggregate(Balance ~ Month, data, mean)
  
  ggplot(data, aes(x = Month, y = Balance)) +
    geom_line(aes(group = Simulation), color = "grey", alpha = 0.1) +
    geom_line(data = avg_balance, aes(x = Month, y = Balance), color = "blue", size = 1) +
    labs(title = "Balance Over Time", x = "Month", y = "Balance ($)") +
    theme_minimal() +
    scale_y_continuous(labels = scales::dollar_format())
}

plot_withdrawals_over_time <- function(data) {
  if (nrow(data) == 0 || ncol(data) < 3) {
    stop("Data is empty or not correctly structured.")
  }
  
  # Calculate the average withdrawal for each month
  avg_withdrawal <- aggregate(Withdrawal ~ Month, data, mean)
  
  ggplot(data, aes(x = Month, y = Withdrawal)) +
    geom_line(aes(group = Simulation), color = "grey", alpha = 0.1) +
    geom_line(data = avg_withdrawal, aes(x = Month, y = Withdrawal), color = "red", size = 1) +
    labs(title = "Withdrawals Over Time", x = "Month", y = "Withdrawal ($)") +
    theme_minimal() +
    scale_y_continuous(labels = scales::dollar_format())
}

get_all_balances <- function(data) {
  if (length(data) == 0) {
    stop("Data is empty.")
  }
  
  total_months <- data[[1]]$total_months
  all_balances <- do.call(cbind, lapply(data, function(x) x$balances))
  
  data.frame(
    Month = rep(1:total_months, times = ncol(all_balances)),
    Balance = as.vector(all_balances),
    Simulation = factor(rep(1:ncol(all_balances), each = total_months))
  )
}

get_all_withdrawals <- function(data) {
  if (length(data) == 0) {
    stop("Data is empty.")
  }
  
  total_months <- data[[1]]$total_months
  all_withdrawals <- do.call(cbind, lapply(data, function(x) x$withdrawals))
  
  data.frame(
    Month = rep(1:total_months, times = ncol(all_withdrawals)),
    Withdrawal = as.vector(all_withdrawals),
    Simulation = factor(rep(1:ncol(all_withdrawals), each = total_months))
  )
}

calculate_contributions <- function(contribution_years, monthly_contribution, annual_increase, mean_inflation) {
  # Ensure contribution_years is a positive integer
  contribution_years <- as.integer(contribution_years)
  if (contribution_years <= 0) {
    stop("contribution_years must be a positive integer")
  }
  
  contribution_years <- seq_len(contribution_years)
  annual_increase_factor <- 1 + annual_increase / 100
  inflation_factor <- 1 + mean_inflation / 100
  contributions_nominal <- numeric(length(contribution_years))
  contributions_inflation_adjusted <- numeric(length(contribution_years))
  
  contributions_nominal[1] <- monthly_contribution * 12  # Annual contribution
  contributions_inflation_adjusted[1] <- contributions_nominal[1]
  
  for (i in 2:length(contributions_nominal)) {
    contributions_nominal[i] <- contributions_nominal[i - 1] * annual_increase_factor
    contributions_inflation_adjusted[i] <- contributions_nominal[i] / (inflation_factor ^ (i-1))
  }
  
  data.frame(
    Year = contribution_years,
    NominalContribution = contributions_nominal,
    InflationAdjustedContribution = contributions_inflation_adjusted
  )
}

create_contrib_table <- function(contributions) {
  contributions
}

print_withdrawal_stats <- function(mean_withdrawal_displayed) {
  cat("Withdrawal Amount during Retirement Period (in Today's Dollars):\n")
  cat("Mean: ", dollar_format()(mean(mean_withdrawal_displayed)), "\n")
  cat("Median: ", dollar_format()(median(mean_withdrawal_displayed)), "\n")
  cat("95% Confidence Interval: [",
      dollar_format()(quantile(mean_withdrawal_displayed, 0.025)), ",",
      dollar_format()(quantile(mean_withdrawal_displayed, 0.975)), "]\n\n")
}

print_balance_stats <- function(balances, title) {
  cat("Statistics at ", title, ":\n")
  cat("Mean: ", dollar_format()(mean(balances)), "\n")
  cat("Median: ", dollar_format()(median(balances)), "\n")
  cat("95% Confidence Interval: [",
      dollar_format()(quantile(balances, 0.025)), ",",
      dollar_format()(quantile(balances, 0.975)), "]\n")
  cat("90% Confidence Interval: [",
      dollar_format()(quantile(balances, 0.05)), ",",
      dollar_format()(quantile(balances, 0.95)), "]\n\n")
}

generate_withdrawal_table <- function(fixed_withdrawal, mean_inflation, contribution_years, retirement_years) {
  total_years <- contribution_years + retirement_years
  years <- seq(contribution_years + 1, total_years)
  annual_withdrawal <- fixed_withdrawal * 12  # Annual withdrawal
  
  nominal_withdrawals <- rep(annual_withdrawal, retirement_years)
  inflation_factor <- cumprod(c(1, rep(1 + mean_inflation/100, retirement_years - 1)))
  inflation_adjusted_withdrawals <- nominal_withdrawals / inflation_factor
  
  mean_inflation_adjusted <- mean(inflation_adjusted_withdrawals)  # Calculate the mean of the inflation-adjusted withdrawals
  
  data <- data.frame(
    Year = years,
    Nominal_Withdrawal = nominal_withdrawals,
    Inflation_Adjusted_Withdrawal = inflation_adjusted_withdrawals
  )
  
  list(
    table_data = data,
    Mean_Inflation_Adjusted_Withdrawal = mean_inflation_adjusted
  )
}


generate_future_nominal_table <- function(fixed_withdrawal, mean_inflation, contribution_years, retirement_years) {
  total_years <- contribution_years + retirement_years
  years <- seq(contribution_years + 1, total_years)
  annual_withdrawal <- fixed_withdrawal * 12  # Annual withdrawal
  
  inflation_factor <- cumprod(c(1, rep(1 + mean_inflation/100, retirement_years - 1)))
  future_nominal_withdrawals <- annual_withdrawal * inflation_factor
  
  data <- data.frame(
    Year = years,
    Today_Dollars = rep(annual_withdrawal, retirement_years),
    Future_Nominal_Dollars = future_nominal_withdrawals
  )
  
  list(
    table_data = data,
    mean_future_nominal = mean(future_nominal_withdrawals)
  )
}

# Run the application
shinyApp(ui = ui, server = server)
