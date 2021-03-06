
# Clear out everything in memory for a nice clean run and easier debugging
rm(list=ls())

##
#
# Logistic regression
# 
# Y_{i} | \beta \sim \textrm{Bin}\left(n_{i},e^{x_{i}^{T}\beta}/(1+e^{x_{i}^{T}\beta})\right)
# \beta \sim N\left(\beta_{0},\Sigma_{0}\right)
#
##

library(mvtnorm)
library(coda)

########################################################################################
########################################################################################
## Handle batch job arguments:

# 1-indexed version is used now.
args <- commandArgs(TRUE)

cat(paste0("Command-line arguments:\n"))
print(args)

####
# sim_start ==> Lowest simulation number to be analyzed by this particular batch job
###

#######################
sim_start <- 1000
length.datasets <- 200
#######################

if (length(args)==0){
  sinkit <- FALSE
  sim_num <- sim_start + 1
  set.seed(1330931)
} else {
  # Sink output to file?
  sinkit <- TRUE
  # Decide on the job number, usually start at 1000:
  sim_num <- sim_start + as.numeric(args[1])
  # Set a different random seed for every job number!!!
  set.seed(762*sim_num + 1330931)
}

# Simulation datasets numbered 1001-1200

########################################################################################
########################################################################################


# Compute posterior probabilities on **log** scale for Binomial data with Normal Prior
log.posterior <- function(n,y,X,beta,mu,Sigma.inv){
  return( t(y) %*% (X %*% beta) - t(n)%*%log(1+exp(X%*%beta)) 
          - 0.5*t(beta-mu) %*% Sigma.inv %*% (beta-mu) )
}


# Main function for the MCMC routine
bayes.logreg <- function(n,y,X,beta.0,Sigma.0.inv,niter,burnin,
                           print.every=1000,retune=100,verbose=TRUE,mu,dims){
  
  # Initialize vectors to store results
  beta.mat = matrix(0, nrow=burnin+niter, ncol=p)
  beta.mat[1,] = t(beta.0)
  accept = logical(length=burnin+niter)
  
  # Initial Variance to be scaled later as necessary
  v = 1
  
  # First burnin and retune
  for (i in 1:burnin){
    
    # Start with the current beta at time t
    beta.t = beta.mat[i,]
    
    # Propose a new beta.star from Normal proposal distribution
    beta.star = rmvnorm(n=1, mean=beta.t, sigma=v*diag(p))
    
    # Compute log posterior probabilities of beta.star and beta.t
    log.posterior.beta.star = log.posterior(n,y,X,t(beta.star),mu.0,Sigma.0.inv)
    log.posterior.beta.t = log.posterior(n,y,X,beta.t,mu.0,Sigma.0.inv)
    log.posterior.beta.t.star  = log.posterior.beta.star - log.posterior.beta.t
    
    # Remember we are on log-scale for the acceptance decision
    alpha = min(0,log.posterior.beta.t.star)
    log.u = log(runif(1))
    
    # Accept new beta.star if alpha > log.u (i.e when alpha is positive since log-scale)
    if (log.u < alpha){
      beta.mat[i+1,] = beta.star
      accept[i] = TRUE
    } else {
      beta.mat[i+1,] = beta.t
    }
    
    # Decide to retune the variance every 100 (or retune) iterations
    # If acceptance rate is too low, lower the variance by the scaling factor
    # If acceptance rate is too high, raise the variance by the scaling factor
    if (i %% retune == 0){
      accept.rate = sum(accept[(i-(retune-1)):i])/retune
      if ( accept.rate < accept.rate.low ){
        v = v/v.scale
      } else if (accept.rate > accept.rate.high){
        v = v*v.scale  
      } # else do not change v
    }
   
    # Print out iteration, acceptance percentage
    if (i %% print.every == 0){
      print(    paste("Iteration: ",i, " Accepted:", accept.rate, sep="")            )
      
    }
  }
  
  # After burnin and tuning, run the main iterations
  for (i in (burnin+1):(burnin+niter-1)){
    
    beta.t = beta.mat[i,]
    
    # Propose a new beta.star from Normal proposal distribution
    beta.star = rmvnorm(n=1, mean=beta.t, sigma=v*diag(p))
    
    # Compute log posterior probabilities of beta.star and beta.t
    log.posterior.beta.star = log.posterior(n,y,X,t(beta.star),mu.0,Sigma.0.inv)
    log.posterior.beta.t = log.posterior(n,y,X,beta.t,mu.0,Sigma.0.inv)
    log.posterior.beta.t.star  = log.posterior.beta.star - log.posterior.beta.t
    
    # Remember we are on log-scale for the acceptance decision
    alpha = min(0,log.posterior.beta.t.star)
    log.u = log(runif(1))
    
    # Accept new beta.star if alpha > log.u (i.e when alpha is positive since log-scale)
    if (log.u < alpha){
      beta.mat[i+1,] = beta.star
      accept[i] = TRUE
    } else {
      beta.mat[i+1,] = beta.t
    }
    
    # Print out iteration, acceptance percentage
    accept.rate = sum(accept[(i-(print.every-1)):i])/print.every
    if (i %% print.every == 0){
      print(    paste("Iteration: ",i, " Accepted:", accept.rate, sep="")            )
      
    }
    
  }
  
  # Return back results
  return(cbind(beta.mat, accept))
}



#################################################
# Set up the specifications:

p = 2 # number of dimensions to pass into the main function
beta.0 <- matrix(rep(1.0,p)) # initialize zero as starting values
Sigma.0.inv <- diag(rep(1.0,p)) # initialize identity for Normal prior covariance matrix
mu.0 = rep(0,p) # initialize mean zero for Normal prior

# setwd("C:/Users/Michael/Documents/Michael UC Davis/STA 250 Adv Stat Computing/HW1")

burnin=10000 # number of iterations to burnin and retune
niter=10000 # number of iterations to run after burnin

# Initialize the target acceptance rate range to pass into the tuning
accept.rate.low = 0.25 
accept.rate.high = 0.60

# Initialize a variance scaling factor to tune the variance of the proposal
v.scale = 1.5

#################################################

# Read data corresponding to appropriate sim_num:
# sim_num = 1200  # just for local testing

# Read in data and Extract X and y:
file = paste("blr_data_",sim_num,".csv", sep="")

dat = read.csv(file, header = TRUE, sep = ",", quote = "\"",
         dec = ".", fill = TRUE, comment.char = "", col.names=c("y","n","X1","X2"))
n = dat$n
y = dat$y
X = cbind(dat$X1,dat$X2)

# Fit the Bayesian model:
beta.est = bayes.logreg(n,y,X,beta.0,Sigma.0.inv,niter,burnin,
                           print.every=1000,retune=100,verbose=FALSE,mu=mu.0, dims=p)

# Extract posterior quantiles...
q = seq(1:99)/100
qtile = cbind(quantile(beta.est[,1], q), quantile(beta.est[,2], q))

# Write results to a (99 x p) csv file...
outfile = paste("blr_res_", sim_num, ".csv", sep="")
write.table(x=qtile,file=outfile, sep=",", col.names=FALSE, row.names=FALSE)
              
# Go celebrate.
 
cat("done. :)\n")


q("no")

