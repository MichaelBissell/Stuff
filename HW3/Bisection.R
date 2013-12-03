# Write a general function to implement the bisection algorithm.
# The function should take the following arguments:
# The function to find the root of
# The initial interval (l,u) (with g(l)g(u)<0
# The tolerance of the convergence criteria
# Maximum number of iterations
# A debugging option (e.g., to print status to the user)


# Clear out the memory for clean run and easier debugging
rm(list=ls())


###############
# Main bisection algorithm
###############
bisection = function(fct, lower, upper, epsilon, max_iter, debug){
  
  converged = FALSE
  iter = 0
  
  while (converged == FALSE & iter < max_iter){
    
    iter = iter + 1
    midpt = (lower+upper)/2
    
    if (debug==TRUE){
      print(paste(iter, " & ", sprintf("%.7f", lower), " & ", sprintf("%.7f", upper), " & ", sprintf("%.7f", midpt), " \\", sep = " "))
    }
    
    if( abs( apply(as.matrix(midpt), 1, fct) ) < epsilon ){
      converged = as.logical(TRUE)
      print("CONVERGED!!!")
    } else if( (apply(as.matrix(lower), 1, fct)*apply(as.matrix(midpt), 1, fct)) < 0 ){
      upper = midpt
    } else {
      lower = midpt
    }
  }
  result = list(converge_status=converged, iter=iter, lower=lower, upper=upper, midpt=midpt)
}


# We start with a likelihood as a function of lambda which we want to maximize.
# But since the max of log likelihood is same as max of likelihood, we switch to log scale for ease.
# Then since we switch from a maximization problem to a root finding problem by taking the derivative.
# So this is our g(x_t) = d/dx log likelihood in lambda and finding these roots is the same 
# as finding the original values of lambda that maximize the original likelihood.
deriv_log_lik_lambda = function(x){
  125/(2+x) - 38/(1-x) + 34/x
}


###############
# Initialize input arguments
###############
fct = deriv_log_lik_lambda      # The functions we want to find the root of
lower = -1                      # The intial lower bound of the interval we want to search
upper = 0                       # The intial upper bound of the interval we want to search
epsilon = 10^(-6)               # The tolerance of the convergence criteria
max_iter = 100                  # Maximum number of iterations
debug = TRUE                    # TRUE or FALSE = A debugging option to print status to the user


###############
# Call the main bisection algorithm and get results
###############
results_bisection = bisection(fct, lower, upper, epsilon, max_iter, debug)
results_bisection










