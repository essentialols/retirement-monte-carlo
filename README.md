# retirement-monte-carlo

An interactive R Shiny app that runs a Monte Carlo simulation of a retirement portfolio: a saving phase with monthly contributions, followed by a withdrawal phase. The simulation core is written in C++ (Rcpp) so thousands of paths run quickly.

**Not financial advice.** This is a learning and exploration tool built on a deliberately simple model (see "Modelling assumptions" and "Known limitations"). Its output is not a forecast and not a recommendation. Do not make financial decisions based on it. Consult a qualified professional.

## What it does

You enter your assumptions in the sidebar and press "Run Simulation". The app simulates many random market and inflation paths month by month and shows:

- histograms of the portfolio balance at retirement and at the end of retirement (mean, median and 95% interval marked), in today's or nominal dollars
- balance and withdrawals over time, every simulated path plus the average, in today's or nominal dollars
- a table of yearly contributions, nominal and in today's dollars
- two deterministic tables for a withdrawal (a constant nominal amount and what it is worth in today's dollars, and an inflation-indexed amount with its nominal value, using the mean inflation input)
- summary statistics in both today's and nominal dollars (mean, median, 95% and 90% intervals) and the share of simulations that end with a negative balance

## Modelling assumptions

All of this is what the code in `simulatePortfolio.cpp` does. Inputs are annual percentages, simulated in monthly steps. The whole simulation runs in **nominal** dollars; results are reported both in nominal dollars and in **today's (real) dollars**, obtained by dividing each nominal amount by the cumulative inflation index of the same month (the index is 1 at the start).

- **Returns:** the "mean return" input is a nominal annual return. Each month's return is drawn independently from a normal distribution with mean `(1 + mean return)^(1/12) - 1` (so twelve average months compound to exactly the annual figure) and standard deviation `sd of return / sqrt(12)` (monthly variances add, so the annualized SD of the monthly returns equals the input). Returns are not correlated with inflation. Note that the SD of a 12-month compounded return is a little higher than the input, because compounding is convex.
- **Inflation:** each month's inflation is drawn independently from a normal distribution with mean `(1 + mean inflation)^(1/12) - 1` and standard deviation `sd of inflation / sqrt(12)`. Monthly draws are compounded into a cumulative inflation index that starts at 1.
- **Contributions:** the monthly contribution is a nominal schedule. It is stepped up by the "annual contribution increase" percentage at the start of each new contribution year (months 13, 25, ...), so year 1 is entirely at the base amount, matching the contributions table, and it is added to the nominal balance as is.
- **Balance:** each month, nominal balance = previous balance x (1 + that month's nominal return) + contribution - withdrawal. The real balance is the nominal balance divided by the cumulative inflation index. The balance is not floored at zero: a depleted portfolio keeps going negative, and the app reports the share of paths that end below zero.
- **Withdrawals** start the month after the contribution years end. Two modes:
  - *Fixed dollar amount:* the amount you enter is in today's dollars (start of the simulation). Each month the nominal withdrawal is that amount times the cumulative inflation index, so its real value stays constant.
  - *Percentage of balance:* the previous nominal balance times `annual rate / 12`.
- **Reported results:** the sidebar switch chooses whether charts (histograms, balance and withdrawal paths) are drawn in today's dollars or in nominal dollars, and every title and axis says which. The text statistics always show both. The average monthly withdrawal is the per-path mean over the withdrawal months only.
- **Tables:** the contribution and withdrawal tables are deterministic and use the mean inflation input, not the random draws, with real amounts expressed in today's dollars (deflated from the start of the simulation).
- **Randomness:** no seed is set, so each run differs. Call `set.seed()` in R before launching if you need repeatable results.

## Known limitations

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
