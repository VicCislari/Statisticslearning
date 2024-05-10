################################################################################
# Example: length of a certain part
# Sample of 100 parts with mean 71: alpha = 0.05 and sigma = 0:4
# file: ex_length_part.R
################################################################################

# Asummption: Normal Distribution

# H0: mu = mu0, H1: mu <> mu0
n <- 100; sigma <- 4; mw <- 71; mu0 <- 70; alpha <- 0.05

# test statistic: mw = sample mean, sigma = standard deviation of the population 
# mw = (X1+...+Xn)/n ~ N(mu0, sigma^2/n) or 

# acceptance region: [L,U] with
# L=alpha/2 quantile of test statistic and U=1-alpha/2 quantil of test statistic
AR <- qnorm(c(alpha/2,1-alpha/2), mean = mu0, sd = sigma/n^0.5)
L <- AR[1]; U <- AR[2]
AR

# decision: reject if value of test statistics is not in AR
(mw < L) | (mw > U)   # true -> reject H0

# p-value: probability that test statistic is not in the interval
# [mu0-(71-mu0),mu0+(71-mu0)] = [69,71] if H0 is true
pnorm(69, mean = mu0, sd = sigma/n^0.5)+1-pnorm(71, mean = mu0, sd = sigma/n^0.5)

# probability of a type II error if mu = 71: probability of 
# L <= test statistic <= U if mu = 71
mu <- 71
pnorm(U, mean = mu, sd = sigma/n^0.5)-pnorm(L, mean = mu, sd = sigma/n^0.5)

# solution with z.test
library(TeachingDemos)
z.test(x = mw, mu = mu0, alternative = "two.sided", sd = sigma, n = n, 
       conf.level = 1-alpha)

# minimal sample size for |mu − 70| ≥ 0.5 and beta(mu) ≤ 0.1
beta <- 0.1
delta <- 0.5
(qnorm(1-alpha/2)+qnorm(1-beta))^2*sigma^2/delta^2

###########################################################################
# now: assumption that the standard deviation of the population is unknown
###########################################################################
n <- 100; s <- 4; mw <- 71; mu0 <- 70; alpha <- 0.05

# test statistic: mw = sample mean, s = standard deviation of the sample
# (mw - mu0)/(s/n^0.5) ~ t-distribution with df = n-1
T <- (mw - mu0)/(s/n^0.5)

# acceptance region: [L,U] with
# L=alpha/2 quantile of the test statistic and 
# U=1-alpha/2 quantile of the test statistic
AR <- qt(c(alpha/2, 1-alpha/2), df = n-1)
L <- AR[1]; U <- AR[2]
AR

# decision: reject if value of test statistics is not in AR
(T < L) | (T > U)   # true -> reject H0

# p-value: probability that test statistic is not in the interval [-|T|,|T|]
pt(-abs(T), df = n-1) + 1-pt(abs(T), df = n-1)  

# probability of a type II error if mu = 71: probability of 
# L <= test statistic <= U if mu = 71     (*)
# Since test statistic = (mw-mu0)/(s/n^0.5) does not follow a t-distribution with df=n-1
# but (mw-mu)/(s/n^0.5) follows a t-distribution with df=n-1
# we must add (m0-mu1)/(s/n^0.5) on both sides of inequality (*)
mu <- 71
beta <- pt((mu0-mu)*sqrt(n)/s+U, df = n-1) - pt((mu0-mu)*sqrt(n)/s+L, df = n-1)
beta
