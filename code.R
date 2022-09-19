source('genetic-algorithm-sol.R')
source('simulated-annealing-sol.R')
max_weight = 500
package_list <- c(
  55, 70,
  31, 32,
  43, 36,
  23, 65,
  48, 49,
  40, 62,
  53, 25,
  54, 47,
  39, 78,
  77, 65,
  39, 23,
  59, 45,
  53, 43,
  28, 44,
  71, 71,
  36, 67,
  62, 48,
  30, 36,
  48, 76,
  42, 2
)
airline_company_genetic_algorithm(max_weight, 
                                  package_list, 
                                  N = 500, 
                                  n = 100)
airline_company_simulated_annealing(max_weight, 
                                    package_list, 
                                    N = 500, 
                                    n = 10, 
                                    temp = 100, 
                                    alpha = 0.9)