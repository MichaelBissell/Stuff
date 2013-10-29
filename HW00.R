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
ggplot(df, aes(x=u, y=v)) + geom_point(alpha = 0.4)

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
# scan() trims whitespace, file() is another option
str_rebuild = ""
for (i in 1:nchar){
  if (i<10){
    filenum = paste("0", i, sep="")
  } else filenum = i
  filename = paste("HW00_Files/out_", filenum, ".txt", sep="")
  temp = readChar(filename, nchars=1)
  str_rebuild = paste(str_rebuild, temp, sep="")
}




# Notes from lecture

%% creat gauss_login.sh in home dir

#!/bin/bash/
ssh -vX mbissell@gauss.cse.ucdavis.edu


chmod u+x gauss_login.sh

