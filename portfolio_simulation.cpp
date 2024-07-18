#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List simulatePortfolio(int numSimulations, double startBalance, double meanReturn, double sdReturn,
                       double meanInflation, double sdInflation, int totalMonths, int contributionMonths,
                       double monthlyContribution, double annualIncrease, double withdrawalRate) {
  List results(numSimulations);
  
  for (int sim = 0; sim < numSimulations; sim++) {
    NumericVector balances(totalMonths);
    NumericVector withdrawals(totalMonths);
    balances[0] = startBalance;
    double currentContribution = monthlyContribution;
    double annualIncreaseFactor = 1 + annualIncrease / 100;
    
    for (int month = 1; month < totalMonths; month++) {
      double monthlyReturn = R::rnorm(meanReturn / 100 / 12, sdReturn / 100 / 12);
      double monthlyInflation = R::rnorm(meanInflation / 100 / 12, sdInflation / 100 / 12);
      
      double contribution = 0;
      if (month <= contributionMonths) {
        contribution = currentContribution;
      }
      
      double withdrawal = 0;
      if (month > contributionMonths) {
        withdrawal = balances[month - 1] * (withdrawalRate / 100 / 12);
      }
      withdrawals[month] = withdrawal;
      
      if ((month - 1) % 12 == 0 && month <= contributionMonths) {
        currentContribution *= annualIncreaseFactor;
      }
      
      double contributionAdjusted = contribution * (1 + monthlyInflation);
      balances[month] = balances[month - 1] * (1 + monthlyReturn) + contributionAdjusted - withdrawal;
    }
    
    double balanceAtRetirement = balances[contributionMonths];
    double balanceAtEnd = balances[totalMonths - 1];
    double meanWithdrawal = mean(withdrawals[Range(contributionMonths, totalMonths - 1)]);
    
    // Store results
    results[sim] = List::create(
      Named("balanceAtRetirement") = balanceAtRetirement,
      Named("balanceAtEnd") = balances[totalMonths - 1],  // Corrected from balance to balances
                                      Named("meanWithdrawal") = meanWithdrawal,
                                      Named("balance") = balances,  // Correct naming to match R's expectation
                                      Named("withdrawals") = withdrawals,  // Already matches
                                      Named("totalMonths") = totalMonths,  // Include if needed in R
                                      Named("contributionMonths") = contributionMonths  // Include if needed in R
    );
  }
  
  return results;
}
