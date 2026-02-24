#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List simulatePortfolio(int numSimulations, double startBalance, double meanReturn, double sdReturn,
                       double meanInflation, double sdInflation, int totalMonths, int contributionMonths,
                       double monthlyContribution, double annualIncrease, bool withdrawalType,
                       double withdrawalRate, double fixedWithdrawal) {
  List results(numSimulations);
  
  for (int sim = 0; sim < numSimulations; sim++) {
    NumericVector balances(totalMonths);
    NumericVector withdrawals(totalMonths);
    balances[0] = startBalance;
    double currentContribution = monthlyContribution;
    double annualIncreaseFactor = 1 + annualIncrease / 100;
    double cumulativeInflation = 1.0; // Start with no inflation
    
    for (int month = 1; month < totalMonths; month++) {
      double monthlyReturn = R::rnorm(meanReturn / 100 / 12, sdReturn / 100 / 12);
      double monthlyInflation = R::rnorm(meanInflation / 100 / 12, sdInflation / 100 / 12);
      cumulativeInflation *= (1 + monthlyInflation); // Update cumulative inflation
      
      double contribution = (month <= contributionMonths) ? currentContribution / cumulativeInflation : 0;
      double withdrawal = 0;
      if (month > contributionMonths) {
        if (withdrawalType) { // If withdrawing a percentage of the balance
          withdrawal = balances[month - 1] * (withdrawalRate / 100 / 12) / cumulativeInflation;
        } else { // If withdrawing a fixed amount
          withdrawal = fixedWithdrawal / (1 + monthlyInflation); // Adjust for inflation each iteration
        }
      }
      withdrawals[month] = withdrawal;
      
      if ((month - 1) % 12 == 0 && month <= contributionMonths) {
        currentContribution *= annualIncreaseFactor;
      }
      
      balances[month] = balances[month - 1] * (1 + monthlyReturn) + contribution - withdrawal;
    }
    
    // Calculate the average withdrawal amount in today's dollars for display
    NumericVector displayedWithdrawals = withdrawals * cumulativeInflation;
    double meanWithdrawalDisplayed = mean(displayedWithdrawals[Range(contributionMonths, totalMonths - 1)]);
    
    results[sim] = List::create(
      Named("balanceAtRetirement") = balances[contributionMonths],
                                             Named("balanceAtEnd") = balances[totalMonths - 1],
                                                                             Named("meanWithdrawalDisplayed") = meanWithdrawalDisplayed, // This is the average displayed withdrawal in today's dollars
                                                                             Named("balances") = balances,
                                                                             Named("withdrawals") = withdrawals,
                                                                             Named("totalMonths") = totalMonths,
                                                                             Named("contributionMonths") = contributionMonths
    );
  }
  
  return results;
}
