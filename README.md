# retirement-monte-carlo

Interactive R Shiny app for retirement portfolio Monte Carlo simulation. Runs thousands of market scenarios against your portfolio to estimate retirement outcomes, with a C++ backend for fast simulation.

## Features

- Monte Carlo simulation of portfolio growth and drawdown
- Configurable inputs: contributions, withdrawal rate, asset allocation, time horizon
- C++ (Rcpp) backend for high-performance simulation
- Multiple app iterations with progressive feature additions
- Interactive Shiny dashboard with visualizations

## Requirements

- R 4.0+
- Packages: `shiny`, `Rcpp`, `ggplot2`
- C++ compiler (for Rcpp)

## Usage

```r
# Run the latest version
shiny::runApp("app3.R")
```

## Files

| File | Purpose |
|------|---------|
| `app.R` | Initial Shiny app |
| `app2.R` | Second iteration with added features |
| `app3.R` | Latest version (run this one) |
| `simulatePortfolio.cpp` | Core C++ simulation engine (Rcpp) |
| `portfolio_simulation2.cpp` | Extended simulation with additional scenarios |
