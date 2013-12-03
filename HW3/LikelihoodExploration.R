
# Clear out the memory for clean run and easier debugging
rm(list=ls())

setwd("C:/Users/Michael/Documents/Michael UC Davis/STA 250 Adv Stat Computing/HW3")

##############################
# Explore the maximum of the likelihood using a grid search as a check
likelihood_lambda = function(x){
  (2+lambda)^125 * (1-lambda)^38 * lambda^34
}

lambda = seq(-1,0,by=0.00001)
lik_lam = likelihood_lambda(lambda)
ind = which(lik_lam == max(lik_lam)) # find which grid search index is the max

pdf("1-Likelihood1.pdf")
plot(lambda, lik_lam, type="l", main="Likelihood on [-1,0]")
abline(h=0)
lines(c(lambda[ind], lambda[ind]), c(0, lik_lam[ind]), lty="dashed")
dev.off()

# initial grid search guess
lambda[ind] 
ind


lambda = seq(0,1,by=0.00001)
lik_lam = likelihood_lambda(lambda)
ind = which(lik_lam == max(lik_lam)) # find which grid search index is the max

pdf("1-Likelihood2.pdf")
plot(lambda, lik_lam, type="l", main="Likelihood on [0,1]")
abline(h=0)
lines(c(lambda[ind], lambda[ind]), c(0, lik_lam[ind]), lty="dashed")
dev.off()

# initial grid search guess
lambda[ind] 
ind
##############################



##############################
# Explore the zero of the derivative of likelihood using a grid search as a check
deriv_likelihood_lambda = function(x){
  g = 125*(2+x)^(124)*(1-x)^(38)*x^34 - 38*(2+x)^125*(1-x)^37*x^34 + 34*(2+x)^125*(1-x)^38*x^33
}

lambda = seq(0.55, 0.65, by=0.0001)
deriv_lik_lam = deriv_likelihood_lambda(lambda)
ind = which(deriv_lik_lam == min(abs(deriv_lik_lam))) # find which grid search index is closest to 0

pdf("1-DerivLikelihood.pdf")
plot(lambda, deriv_lik_lam, type="l", main="Derivative of Likelihood")
abline(h=0)
points(lambda[ind], deriv_lik_lam[ind], pch=19, col="red", cex=2)
dev.off()

lambda[ind]
ind
##############################



##############################
# Explore the maximum of the log likelihood using a grid search as a check
log_likelihood_lambda = function(x){
  g = 125*log(2+x) + 38*log(1-x) + 34*log(x)
}

lambda = seq(0,1,by=0.00001)
log_lik_lam = log_likelihood_lambda(lambda)
ind = which(log_lik_lam == max(log_lik_lam))
pdf("1-loglikelihood.pdf")
plot(lambda, log_lik_lam, type="l", ylim=c(-100,100), main="Log Likelihood")
abline(h=0)
lines(c(lambda[ind], lambda[ind]), c(0, log_lik_lam[ind]), lty="dashed")
dev.off()

lambda[ind]
ind
##############################



##############################
# Explore the zero of the derivative of the log likelihood using a grid search as a check
deriv_log_lik_lambda = function(x){
  g = 125/(2+x) - 38/(1-x) + 34/x
}

lambda = seq(-5,5,by=0.001)
deriv_log_lik_lam = deriv_log_lik_lambda(lambda)
ind = which(deriv_log_lik_lam == min(abs(deriv_log_lik_lam)))

pdf("1-DerivLogLikelihood.pdf")
plot(lambda, deriv_log_lik_lam, type="l", ylim=c(-1000,1000), 
     main="Derivative of Log-Likelihood")
abline(h=0)
abline(v=-2, lty="dashed")
abline(v=0, lty="dashed")
abline(v=1, lty="dashed")
dev.off()


lambda[ind]
ind
##############################
