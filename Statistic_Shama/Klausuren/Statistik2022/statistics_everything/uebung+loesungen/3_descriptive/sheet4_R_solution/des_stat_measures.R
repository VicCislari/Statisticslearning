#####################################################
# Descriptive Statistics: Functions for different 
# measures and their implentations in R
#
# File: des_stat_measures.R
#
#####################################################
library(tidyverse)

# sample 
x <- c(3, 7, 2, 5, 6, 10, 6, 3, 6, 5)
sort_x <- sort(x)


# my measures: mean, quantile, variance, geometric mean,
# harmonic mean, trimmed mean
my_mean <- function(x) {
  return(sum(x)/length(x))
}
my_quantile <- function(x,p) {
  x_sort <- sort(x)
  return(x_sort[ceiling(length(x)*p)])
}
my_var <- function(x) {
  mean_x <- sum(x)/length(x)
  return(sum((x-mean_x)^2)/(length(x)-1))
}
geo_mean <- function(x) {
  return(prod(x)^(1/length(x)))
}
harm_mean <- function(x) {
  return(length(x)/(sum(1/x)))
}
trim_mean <- function(x,p) {
  # remove the fraction p/2 of observations from each 
  # end of the ordered sample
  n.remove <- floor(length(x)*p/2) 
  x.ordered <- sort(x)
  x.ordered[(1+n.remove): (length(x)-n.remove)] %>%
    mean() %>% 
    return()
}
# corresponds mean(x, trim = p/2)
	
# several measures for the sample x
# mean
mean(x)
my_mean(x)
# quantiles: type 1 and type 7 methods
quantile(x,c(0.25,0.5,0.75),type=1)
quantile(x,c(0.25,0.5,0.75), type = 7) # = quantile(x,c(0.25,0.5,0.75))
my_quantile(x,c(0.25,0.5,0.75))
# variance
var(x)
my_var(x)
# geometric mean
geo_mean(x)
# harmonic mean
harm_mean(x)
# trimmed mean
trim_mean(x,0.2)
mean(x, trim = 0.1)

# R quantiles: type=1
# If np is an integer p-quantil is x_(np)
# If np is not an integer p-quantil is x_(ceiling(np))
# This is equivalent to the definition from the lecture
#
# R quantiles: type=7
# Consider the points ((k-1)/(n-1), x_(k)) for k=1,2,...,n-1 with x_(k) the 
# k-th value of the ordered sample. Let f() be the function defined by 
# linear interpolating these points. Then the p quantile is f(p)

my_q7 <- function(x,p) {
  x.ordered <- sort(x)
  h <- (length(x)-1)*p +1
  return(x.ordered[floor(h)] + (h-floor(h))*
           (x.ordered[floor(h)+1]-x.ordered[floor(h)]))
}

# compare the values of my_q7 and R, type = quantile
# my_q7(x,seq(0.05,0.95, by=0.01)) - quantile(x,seq(0.05,0.95, by=0.01))
plot(x=(1:length(x)-1)/(length(x)-1),y=sort(x),
     type="b", col = "red",
     xlim=c(0,1.11), ylim=c(0,10),
     xlab="p", ylab="p quantile",
     main="quantiles type=7")
lines(x=seq(0.01,0.99, by=0.01), y=my_q7(x,seq(0.01,0.99, by=0.01)), col="red")


dev.copy2eps(file="../pictures/median_type7.eps")

plot.ecdf(x,xlab="p quantile", ylab="p",
          main="quantiles type=1",
          sub="inverse of the empirical distribution function")
dev.copy2eps(file="../pictures/median_type1.eps")