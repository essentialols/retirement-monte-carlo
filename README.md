# retirement-monte-carlo

An interactive R Shiny app that runs a Monte Carlo simulation of a retirement portfolio: a saving phase with monthly contributions, followed by a withdrawal phase. The simulation core is written in C++ (Rcpp) so thousands of paths run quickly.

**Not financial advice.** This is a learning and exploration tool built on a deliberately simple model (see "Modelling assumptions" and "Known limitations"). Its output is not a forecast and not a recommendation. Do not make financial decisions based on it. Consult a qualified professional.

## What it does

You enter your assumptions in the sidebar and press "Run Simulation". The app simulates many random market and inflation paths month by month and shows:

- histograms of the portfolio balance at retirement and at the end of retirement (mean, median and 95% interval marked)
- balance and withdrawals over time, every simulated path plus the average
- a table of yearly contributions, nominal and inflation-adjusted
- two deterministic tables for a fixed withdrawal (what it is worth in today's dollars, and what it would be in future nominal dollars, using the mean inflation input)
- summary statistics (mean, median, 95% and 90% intervals) and the share of simulations that end with a negative balance

## Modelling assumptions

All of this is what the code in `simulatePortfolio.cpp` does. Inputs are annual percentages, simulated in monthly steps.

- **Returns:** each month's return is drawn independently from a normal distribution with mean `mean return / 12` and standard deviation `sd of return / 12`. Returns are not correlated with inflation.
- **Inflation:** each month's inflation is drawn independently from a normal distribution with mean `mean inflation / 12` and standard deviation `sd of inflation / 12`. Monthly draws are compounded into a cumulative inflation index that starts at 1.
- **Contributions:** the monthly contribution is stepped up by the "annual contribution increase" percentage once a year during the contribution years. Each contribution is divided by the cumulative inflation index before it is added to the balance.
- **Balance:** each month, balance = previous balance x (1 + that month's return) + contribution - withdrawal. The balance is not floored at zero: a depleted portfolio keeps going negative, and the app reports the share of paths that end below zero.
- **Withdrawals** start the month after the contribution years end. Two modes:
  - *Fixed dollar amount:* the monthly amount you enter, divided by (1 + that month's inflation draw).
  - *Percentage of balance:* the previous balance times `annual rate / 12`, divided by the cumulative inflation index.
- **Reported withdrawal "in today's dollars":** per path, the monthly withdrawals are multiplied by the cumulative inflation index at the end of the path and averaged over the retirement months. The mean, median and 95% interval of that figure across paths are shown.
- **Tables:** the contribution and fixed-withdrawal tables are deterministic and use the mean inflation input, not the random draws.
- **Randomness:** no seed is set, so each run differs. Call `set.seed()` in R before launching if you need repeatable results.

## Known limitations

- The monthly standard deviation is the annual input divided by 12 (not by the square root of 12), so the effective annual volatility of the simulated returns is lower than the number you type in.
- Contributions and percentage withdrawals are deflated by the inflation index while returns are applied in nominal terms, and fixed withdrawals are adjusted by a single month's inflation. Real and nominal amounts are therefore mixed, and the results should be read as rough illustrations rather than precise dollar values.
- No taxes, fees, sequence-of-returns modelling beyond independent draws, fat tails, mean reversion, social security or other income.

## Requirements

- R 4.0 or newer
- R packages: `shiny`, `shinythemes`, `ggplot2` (3.4 or newer), `scales`, `Rcpp`
- A C++ compiler that Rcpp can use (Xcode command line tools on macOS, Rtools on Windows, `build-essential` on Linux)

```r
install.packages(c("shiny", "shinythemes", "ggplot2", "scales", "Rcpp"))
```

## Run

From the repository folder:

```r
shiny::runApp("app.R")
```

The C++ code is compiled the first time the app starts.

## Files

| File | Purpose |
|------|---------|
| `app.R` | The Shiny app (UI, server, plotting and table helpers) |
| `simulatePortfolio.cpp` | Rcpp simulation engine used by the app |

## License

MIT, see `LICENSE`.
