# Michael Bissell
# STA 250 Fall 2013 Prof. Baines
# HW 00


rm(list=ls())
setwd("C:/Users/Michael/Documents/Michael UC Davis/STA 250 Adv Stat Computing")


##########
# #1 Fizzbuzz problem
##########

# First attempt
for(i in 1:100){
  tag = i
  if ((i %% 3)==0 && ((i%%5)==0)){
    tag = "fizzbuzz"
  } else if((i%%3)==0 && ((i%%5)!=0)){
    tag = "buzz"
  } else if(((i%%3)!=0) && ((i%%5)==0)){
    tag = "fizzbuzz"
  }
  tag = paste(i, " ", tag)
  print(tag)
  i
}


# Second attempt (can simplify the 2nd and 3rd else if)
for(i in 1:100){
  tag = i
  if ((i %% 3)==0 && ((i%%5)==0)){
    tag = "fizzbuzz"
  } else if( i %% 3 == 0){
    tag = "fizz"
  } else if( i %% 5 == 0){
    tag = "buzz"
  }
  tag = paste(i, " ", tag)
  print(tag)
  i
}
  

##########
# 2 runif
##########
Write a program that generates 10,000 uniform random numbers between 0 and equation 
(call this equation), and 10,000 uniform random numbers between 0 and 1 (call this equation). 
You will then have 10,000 pairs of random numbers.
Transform equation to equation where: equation, and, equation.
Make a 2D scatterplot of the 10,000 (u,v) pairs. What is the distribution of: equation?

x = runif(10000, 0, 2*pi)
y = runif(10000)
u = y*cos(x)
v = y*sin(x)


# Standard scatterplot is too high desity so use ggplot
plot(u,v) 

library(ggplot2)
df = data.frame(u,v)
ggplot(df, aes(x=u, y=v)) + geom_point(alpha = 0.2)

library(hexbin)
bin<-hexbin(u, v, xbins=50)
bin<-hexbin(u, v)
plot(bin, main="Hexagonal Binning") 

# What is the distribution of r = sqrt(u^2 + v^2) ?
r = sqrt(u^2 + v^2) = sqrt(y^2*cos^2(x) + y^2*sin^2(x)) = sqrt(y^2* (cos^2(x) + sin^2(x)))
  = sqrt(y^2* 1) = y = Uniform(0,1)

# Double check by plot
r = sqrt(u^2 + v^2)
hist(r, freq=FALSE)


##########
# 3 parsing text and files
##########

str = "Hello, my name is Bob. I am a statistician. I like statistics very much."
nchar = nchar(str)

# use sink() and cat() to output to files
for (i in 1:nchar){
  if (i<10){
    filenum = paste("0", i, sep="")
  } else filenum = i
  filename = paste("HW00_Files/out_", filenum, ".txt", sep="")
  sink(filename, append=TRUE, split=TRUE)
  cat(substr(str,i,i))
  sink()
}

# use readChar() to read in the file contents which maintains whitespace
# scan() trims whitespace, file() is another option, Duncan said readLines()
str_rebuild = ""
for (i in 1:nchar){
  if (i<10){
    filenum = paste("0", i, sep="")
  } else filenum = i
  filename = paste("HW00_Files/out_", filenum, ".txt", sep="")
  temp = readChar(filename, nchars=1)
  str_rebuild = paste(str_rebuild, temp, sep="")
}


##########
# 4 Run sarray boot_camp_demo.py
##########

Done


##########
# 5 Twitter code
##########

NOT Done


##########
# 6 AR Models
##########
n = 1000
nbatch = 200

time = rep(1:n, nbatch)
batch = rep(1:nbatch, each=n)
rho = 0.9

y = numeric(0)
for (j in 1:nbatch){
  x = numeric(n)
  e = rnorm(n)
  for (i in 2:n){
    x[i] = rho*x[i-1] + e[i]
  }
  y = c(y,x)
}

df = data.frame(time, batch, y)

# try smoothscatter()
library(ggplot2)
ggplot(df, aes(time, y, group=batch)) + geom_line(alpha = 0.05) + xlab("t") + ylab("y")



mat = matrix(y, nrow=n, ncol=nbatch)

# c. Compute the mean of the 200 realizations at each time points t=1,2,...,1000.
# Plot the means.
rmean = apply(mat, 1, mean) # means by rows
plot(rmean, type="l")

# d. Plot the variance of the 200 realizations at each time points t=1,2,...,1000.
# Plot the variances.
rvar = apply(mat, 1, var) # variance by rows
plot(rvar, type="l")

# e. Compute the mean of each of the 200 series across time points i=1,2,...,200.
# Plot the means.
cmean = apply(mat, 2, mean) # means by columns
plot(cmean, type="l")

# f. Compute the variance of each of the 200 series across time points i=1,2,...,200.
# Plot the variances.
cvar = apply(mat, 2, var) # variance by columns
plot(cvar, type="l")

# g. Justify the results you have seen in parts b.--f. theoretically.
X_t ~ IIDNoise(mean = 0)
e_t ~ WhiteNoise(mean=0, var=sigma^2=1)

E[X_t] = E[ rho*X_t-1 + e_t ] = rho*E[X_t-1] + E[ e_t ] = rho*0 + 0 = 0

Var[X_t] = Var[ rho*X_t-1 + e_t ] = rho^2*Var[X_t-1] + Var[ e_t ] 
         = rho^2*Var[X_t] + sigma^2
  --> Var(X_t) = sigma^2 / (1-rho^2) = 1/(1-0.9^2) = 5.26




##########
# 7 Monte Carlo Integration
##########

# a. Let Z ~N(0,1). Compute E[exp-Z2] using Monte Carlo integration.
z = rnorm(100000)
mean( exp(-z^2) )

# b. Let Z Truncated-Normal(0,1;[-2,1]). Compute E[Z] using importance sampling.
library(msm)
trunc_norm = rtnorm(100000, mean=0, sd=1, lower=-2, upper=1)
mean(trunc_norm)


s = runif(100000, -2, 1)
f = dtnorm(s)
g = dunif(s,-2,1)
sum(s*f/g)

# Notes from lecture

%% creat gauss_login.sh in home dir

#!/bin/bash/
ssh -vX mbissell@gauss.cse.ucdavis.edu


chmod u+x gauss_login.sh

