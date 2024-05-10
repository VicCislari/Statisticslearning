2############################################################
# R offers a number of functions for calculating with normal
# distributions. Call them up in the RStudio Help area with 
# the keyword Normal and familiarize yourself with them.
# R has four in built functions to generate normal distribution.
# 
# file: prob_nd_rfunctions.R
#############################################################

# The function dnorm() gives height of the probability density
# at each point for a given mean and standard deviation. Apply 
# this function to create a plot the density of the normal 
# distribution with mean 2.5 and standard deviation 1.5.
x <- seq(-10, 10, by = .1)
y <- dnorm(x, mean = 2.5, sd = 1.5)
plot(x,y, type = "l")

# pnorm() gives the probability of a normally distributed random 
# variable to be less that the value of a given number (cumulative
# distribution function). Apply this function to create a plot of
# the normal distribution function with mean 2.5 and standard 
# deviation 1.5.
x <- seq(-10,10,by = .1)
y <- pnorm(x, mean = 2.5, sd = 1.5)
plot(x,y, type = "l")

# qnorm() takes the probability value and gives a number whose
# cumulative value matches the probability value (quantile). 
# Apply this function to plot the quantiles of the normal distribution
# with mean 2.5 and standard deviation 1.5.
x <- seq(0, 1, by = 0.02)
y <- qnorm(x, mean = 2, sd = 1.5)

plot(x,y,type = "l")

# rnorm() is used to generate random numbers whose distribution is
# normal. It takes the sample size as input and generates that many 
# random numbers. Draw a histogram to show the distribution of the 
# generated numbers which are normally distributed with mean 2.5 and 
# standard deviation 1.5.
y <- rnorm(100,mean = 2.5, sd = 1.5)
hist(y, main = "Normal Distribution (mean=2.5, sd=1.5)")
