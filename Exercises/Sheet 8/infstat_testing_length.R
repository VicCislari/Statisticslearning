###################################################################
# The length of a certain machined part is supposed to be 10
# centimeters. In fact, due to imperfections in the manufacturing
# process, the actual length is a random variable. The standard
# deviation is due to inherent factors in the process, which remain
# fairly stable over time. From historical data, the standard
# deviation is known with a high degree of accuracy to be 0.3. The
# mean, on the other hand, may be set by adjusting various parameters
# in the process and hence may change to an unknown value fairly
# frequently. We are interested in testing
# H_0: mu = 10 versus H_1: mu <> 10 
# resp.
# H_0: mu <= 10 versus H_1: mu > 10 
# 
# Hint: Assume that the data is approximately normally distributed.
# file: infstat_testing_length.R
####################################################################
library(tidyverse)
# H_0: mu = 10 versus H_1: mu <> 10 
sigma <- 0.3
hyp.mean <- 10

# a) Suppose that a sample of 100 parts has mean 10.1. Perform the
# test at the 0.1 level of significance.
sample.size <- 100
sample.mean <- 10.1
alpha <- 0.1
# Test statistic
T <- (sample.mean - hyp.mean)*sqrt(sample.size)/sigma
T
# rejection region: T < lb or T > ub
qnorm(c(alpha/2,1-alpha/2)) # -1.64, 1.64 -> reject H_0

# b) Compute the p-value for the data.
1-pnorm(abs(T)) + pnorm(-abs(T))

# c) Compute the probability of a type II error beta of the
# test at mu = 10.05.
beta <- function(mu1,mu0,n,alpha,sigma) {
  # rejection bounds
  lb <- mu0-sigma*qnorm(1-alpha/2)/sqrt(n)
  ub <- mu0+sigma*qnorm(1-alpha/2)/sqrt(n)
  # prob. of type II error
  beta <- pnorm(ub, mean = mu1, sd = sigma/sqrt(n)) - 
    pnorm(lb, mean = mu1, sd = sigma/sqrt(n))
  return(beta)
}
beta(10.05, hyp.mean,sample.size,alpha, sigma)

# d) Compute the approximate sample size needed for significance
# level 0.1 and beta = 0.2 when mu = 10.05.      	
tibble(
  n = 100:500,
  p.II = beta(10.05, hyp.mean,n,alpha, sigma)
) %>% filter(p.II <= 0.2)

# approximated value
((qnorm(1-alpha/2)-qnorm(0.2))*sigma/(10.05-hyp.mean))^2

# e) plot the prob. of a type II error depending on the value of mu for different
# values of the sample size n = 50, 100, 150, 200, 250
plot(x=seq(9.8,10.2,by=0.005),
     y=beta(seq(9.8,10.2,by=0.005), hyp.mean,sample.size,alpha, sigma),
     type="l",
     main = "probability of a type II error",
     sub = "blue n=50, black n=100, red n=150,200,250",
     xlab = "mu", ylab = "beta")
lines(x=seq(9.8,10.2,by=0.005),
      y=beta(seq(9.8,10.2,by=0.005), hyp.mean,50,alpha, sigma),
      type="l", col="blue")
lines(x=seq(9.8,10.2,by=0.005),
      y=beta(seq(9.8,10.2,by=0.005), hyp.mean,150,alpha, sigma),
           type="l", col="red")
lines(x=seq(9.8,10.2,by=0.005),
      y=beta(seq(9.8,10.2,by=0.005), hyp.mean,200,alpha, sigma),
      type="l", col="red")
lines(x=seq(9.8,10.2,by=0.005),
      y=beta(seq(9.8,10.2,by=0.005), hyp.mean,250,alpha, sigma),
      type="l", col="red")

# f) Show that the H_0 will be not rejected if the sample mean is 10.01. Determine
# the smallest n for the p-value for a sample mean 10.01 is less than 0.001. 
# In general by increasing the sample size every small sample mean will become "highly
# significant - p value < 0.001"
pvalue <- function(n,s.mean,mean.0) {
  # value of the test statistic
  TS <- (s.mean - mean.0)*sqrt(n)/sigma
  return(1-pnorm(abs(TS))+pnorm(-abs(TS)))
}
# p values for the sample means 10.1 and 10.01 are 0.0008581207 and 0.7388827 
pvalue(sample.size,10.1,hyp.mean)
pvalue(sample.size,10.01,hyp.mean)
df.pvalue <- tibble(
  n = sample.size:10000,
  p = pvalue(n,10.01,hyp.mean)
)
df.pvalue %>% filter(p <= 0.001) # n >= 9745

# exact
(30*qnorm(0.9995))^2
