#include <Rcpp.h>
using namespace Rcpp;

// Everything is simulated in NOMINAL dollars. Real ("today's dollars") series are
// obtained by dividing a nominal amount by the cumulative inflation index of the
// same month (index = 1 at month 0).
//
// Inputs are annual percentages. The monthly step uses:
//   mean: geometric conversion, (1 + annual)^(1/12) - 1, so twelve average months
//         compound to exactly the annual figure
//   sd:   annual sd / sqrt(12), since independent monthly variances add
//
// Return and inflation inputs are nominal annual figures. The monthly contribution
// (and its annual step-up) is a nominal schedule. The fixed withdrawal is entered in
// today's dollars (month 0) and is grown by the cumulative inflation index each
// month, so its real value stays constant.
// [[Rcpp::export]]
List simulatePortfolio(int numSimulations, double startBalance, double meanReturn, double sdReturn,
                       double meanInflation, double sdInflation, int totalMonths, int contributionMonths,
                       double monthlyContribution, double annualIncrease, bool withdrawalType,
                       double withdrawalRate, double fixedWithdrawal) {
  List results(numSimulations);

  const double monthlyMeanReturn = std::pow(1 + meanReturn / 100, 1.0 / 12) - 1;
  const double monthlySdReturn = sdReturn / 100 / std::sqrt(12.0);
  const double monthlyMeanInflation = std::pow(1 + meanInflation / 100, 1.0 / 12) - 1;
  const double monthlySdInflation = sdInflation / 100 / std::sqrt(12.0);

  for (int sim = 0; sim < numSimulations; sim++) {
    NumericVector balances(totalMonths);          // nominal
    NumericVector balancesReal(totalMonths);      // today's dollars
    NumericVector withdrawals(totalMonths);       // nominal
    NumericVector withdrawalsReal(totalMonths);   // today's dollars
    NumericVector inflationIndex(totalMonths);    // cumulative inflation, 1 at month 0
    balances[0] = startBalance;
    balancesReal[0] = startBalance;
    inflationIndex[0] = 1.0;
    double currentContribution = monthlyContribution;
    double annualIncreaseFactor = 1 + annualIncrease / 100;
    double cumulativeInflation = 1.0;

    for (int month = 1; month < totalMonths; month++) {
      double monthlyReturn = R::rnorm(monthlyMeanReturn, monthlySdReturn);
      double monthlyInflation = R::rnorm(monthlyMeanInflation, monthlySdInflation);
      cumulativeInflation *= (1 + monthlyInflation);
      inflationIndex[month] = cumulativeInflation;

      double contribution = (month <= contributionMonths) ? currentContribution : 0;
      double withdrawal = 0;
      if (month > contributionMonths) {
        if (withdrawalType) { // percentage of the (nominal) balance
          withdrawal = balances[month - 1] * (withdrawalRate / 100 / 12);
        } else { // fixed amount in today's dollars, indexed by cumulative inflation
          withdrawal = fixedWithdrawal * cumulativeInflation;
        }
      }
      withdrawals[month] = withdrawal;
      withdrawalsReal[month] = withdrawal / cumulativeInflation;

      if ((month - 1) % 12 == 0 && month <= contributionMonths) {
        currentContribution *= annualIncreaseFactor;
      }

      balances[month] = balances[month - 1] * (1 + monthlyReturn) + contribution - withdrawal;
      balancesReal[month] = balances[month] / cumulativeInflation;
    }

    // Average monthly withdrawal over the withdrawal months (those after contributionMonths)
    double meanWithdrawal = mean(withdrawals[Range(contributionMonths + 1, totalMonths - 1)]);
    double meanWithdrawalReal = mean(withdrawalsReal[Range(contributionMonths + 1, totalMonths - 1)]);

    results[sim] = List::create(
      Named("balanceAtRetirement") = balances[contributionMonths],
      Named("balanceAtRetirementReal") = balancesReal[contributionMonths],
      Named("balanceAtEnd") = balances[totalMonths - 1],
      Named("balanceAtEndReal") = balancesReal[totalMonths - 1],
      Named("meanWithdrawal") = meanWithdrawal,
      Named("meanWithdrawalReal") = meanWithdrawalReal,
      Named("balances") = balances,
      Named("balancesReal") = balancesReal,
      Named("withdrawals") = withdrawals,
      Named("withdrawalsReal") = withdrawalsReal,
      Named("inflationIndex") = inflationIndex,
      Named("totalMonths") = totalMonths,
      Named("contributionMonths") = contributionMonths
    );
  }

  return results;
}
