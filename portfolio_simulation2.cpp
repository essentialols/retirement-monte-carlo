#include <Rcpp.h>
#include <random>
#include <numeric>
#include <algorithm>

// [[Rcpp::export]]
Rcpp::List simulatePortfolio(double meanReturn, double sdReturn,
                             double meanInflation, double sdInflation,
                             double startBalance, double monthlyContribution,
                             double annualIncrease, int contributionYears,
                             int retirementYears, std::string withdrawalType,
                             double fixedWithdrawal, double withdrawalRate,
                             int numSimulations) {
  
  std::mt19937 rng; // Random number generator (seed for reproducibility)
  std::normal_distribution<> returnDist(meanReturn / 100 / 12, sdReturn / 100 / 12);
  std::normal_distribution<> inflationDist(meanInflation / 100 / 12, sdInflation / 100 / 12);
  
  int totalMonths = (contributionYears + retirementYears) * 12;
  
  Rcpp::NumericMatrix contributions(numSimulations, totalMonths);
  Rcpp::NumericMatrix withdrawals(numSimulations, totalMonths);
  Rcpp::NumericVector balancesAtRetirement(numSimulations);
  Rcpp::NumericVector balancesAtEnd(numSimulations);
  Rcpp::NumericVector meanWithdrawalDisplayed(numSimulations);
  
  for (int sim = 0; sim < numSimulations; ++sim) {
    double balance = startBalance;
    double currentContribution = monthlyContribution;
    double cumulativeInflation = 1.0; // to keep track of cumulative inflation effect
    
    for (int month = 0; month < totalMonths; ++month) {
      // Update cumulative inflation
      double inflation = inflationDist(rng);
      cumulativeInflation *= (1 + inflation);
      
      // Contributions phase
      if (month < contributionYears * 12) {
        if (month % 12 == 0 && month > 0) { // Apply annual increase at the start of each year
          currentContribution *= (1 + annualIncrease / 100);
        }
        contributions(sim, month) = currentContribution;
        balance += currentContribution; // Add contribution before applying return
      }
      
      // Apply return on the current balance
      double returnRate = returnDist(rng);
      balance *= (1 + returnRate);
      
      // Withdrawals phase
      if (month >= contributionYears * 12) {
        double withdrawalAmount = (withdrawalType == "fixed") ?
        fixedWithdrawal * cumulativeInflation : // Escalate fixed withdrawal by cumulative inflation
        balance * (withdrawalRate / 100 / 12);
        
        withdrawals(sim, month) = withdrawalAmount;
        balance -= withdrawalAmount;
      }
    } 
    
    balancesAtRetirement[sim] = balance;  // Balance at the start of retirement
    balancesAtEnd[sim] = balance;
    meanWithdrawalDisplayed[sim] = std::accumulate(withdrawals.begin() + contributionYears * 12, withdrawals.end(), 0.0) / (retirementYears * 12);
  }
  
  // Calculate percentiles for confidence intervals
  std::sort(balancesAtRetirement.begin(), balancesAtRetirement.end());
  std::sort(balancesAtEnd.begin(), balancesAtEnd.end());
  std::sort(meanWithdrawalDisplayed.begin(), meanWithdrawalDisplayed.end());
  
  return Rcpp::List::create(
    Rcpp::Named("contributions") = contributions,
    Rcpp::Named("withdrawals") = withdrawals,
    Rcpp::Named("balancesAtRetirement") = balancesAtRetirement,
    Rcpp::Named("balancesAtEnd") = balancesAtEnd,
    Rcpp::Named("meanWithdrawalDisplayed") = meanWithdrawalDisplayed,
    Rcpp::Named("probBelowZero") = std::count_if(balancesAtEnd.begin(), balancesAtEnd.end(), [](double balance) { return balance < 0; }) / static_cast<double>(numSimulations)
  ); 
}
