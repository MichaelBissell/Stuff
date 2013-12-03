# Write a general function to implement the Newton-Raphson algorithm.
# The function should take the following arguments:
# The function to find the root of
# The derivative of the function
# The starting value
# The tolerance of the convergence criteria
# Maximum number of iterations
# A debugging option (e.g., to print status to the user)
 

# Clear out the memory for clean run and easier debugging
rm(list=ls())


###############
# Define the main Newton-Raphson algorithm
###############
newtonraphson = function(fct, deriv, initial, epsilon, max_iter, debug){

  converged = FALSE
  iter = 0
  x_t = initial
  while (converged == FALSE & iter < max_iter){
    
    iter = iter + 1
    
    # Compute values of g(x_t) and g'(x_t)
    g = apply(as.matrix(x_t), 1, fct)
    g_prime = apply(as.matrix(x_t), 1, deriv)
    
    # Check if g(x_t) and g'(x_t) diverged to +/-Inf, otherwise compute x_(t+1)
    if (!is.finite(g) | !is.finite(g_prime)){
      converged = "OOOPS, DIVERGED!!!"
      print("OOOPS, DIVERGED!!!")
      results = list(converge_status=converged, iter=iter, g, g_prime)
      return(results)
    } else {
      x_tp1 = x_t - g/g_prime
      
      if (debug==TRUE){
        print(paste(iter, " & ", sprintf("%.7f", x_t), " & ", sprintf("%.7f", g), " & ", sprintf("%.7f", g_prime), " & ", 
                    sprintf("%.7f", x_tp1),  " \\", sep = " "))
      }
      
      # If X_(t+1) is within epsilon of the root, then we are done, else increment x_t --> x_(t+1)
      if( abs( apply(as.matrix(x_tp1), 1, fct) ) < epsilon ){
        converged = as.logical(TRUE)
        print("CONVERGED!!!")
      } else {
        x_t = x_tp1
      }
    }
  }
  results = list(converge_status=converged, iter=iter, root=x_tp1)
  return(results)
}


# We start with a likelihood as a function of lambda which we want to maximize.
# But since the max of log likelihood is same as max of likelihood, we switch to log scale for ease.
# Then since we switch from a maximization problem to a root finding problem by taking the derivative.
# So this is our g(x_t) = d/dx log likelihood in lambda and finding these roots is the same 
# as finding the original values of lambda that maximize the original likelihood.
deriv_log_lik_lambda = function(x){
  125/(2+x) - 38/(1-x) + 34/x
}


# As commented above, this is our g'(x_t) = second derivative of log likelihood in lambda
deriv_deriv_log_lik_lambda = function(x){
  -125/(2+x)^2 - 38/(1-x)^2 - 34/x^2
}


###############
# Initialize input arguments
###############
fct = deriv_log_lik_lambda            # function we want to find root of
deriv = deriv_deriv_log_lik_lambda    # derivative of the function 
initial = -0.1                         # starting value
epsilon = 10^(-6)                     # tolerance of the convergence criteria
max_iter = 100                        # Maximum number of iterations
debug = TRUE                          # TRUE or FALSE = A debugging option to print status to the user

###############
# Call the Newton-Raphson algorithm and get results
###############
results_newtonraphson = newtonraphson(fct, deriv, initial, epsilon, max_iter, debug)
results_newtonraphson
