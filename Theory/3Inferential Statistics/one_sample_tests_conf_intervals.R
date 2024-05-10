##########################################################################
#
# Examples of one random sample test and confidence intervals
#
# Tests, confidence interval in case of normal distributions
# (a) expected value, variance known
# (b) expected value, variance unknown
# (c) variance, expected value unknown
#
# Tests, confidence intervals in case of binomial distributions
#
# In every case only two-tailed test and two sided confidence intervals are
# considered. Furthermore the formulas given in the slides are directly 
# applied and additionally the results are calculated by suitable R functions.
#
# file one_sample_test_conf_intervals.R
##########################################################################

set.seed(12345) # set the seed for the random number generator

# generate a random sample
n <- 25; mu <- 100; stdev <- 5
x <- rnorm(n,mu,stdev)
x

# significance level
alpha <- 0.05

##########################################################################
# (a) expected value, variance known
##########################################################################
# direct calculation
sigma <- 5
q <- qnorm(1-alpha/2)
mv <- mean(x)
# confidence interval [l; u]
l <- mv - q * sigma / sqrt(n)
u <- mv + q * sigma / sqrt(n)
l; u # 98.03415; 101.9541
# test H0: mu = 99
mu0 <- 99
test.stat <- (mv - mu0)*sqrt(n)/sigma
# reject H0, if test.stat < -qnorm(1-alpha/2) or test.stat > qnorm(1-alpha/2)
(test.stat < -qnorm(1-alpha/2)) | (test.stat > qnorm(1-alpha/2))
# false -> no rejection
# pvalue
pval <- 1-(pnorm(abs(test.stat))-pnorm(-abs(test.stat)))
pval # 0.3201676
# test and confidence interval using z.test from the TeachingDemos library
library(TeachingDemos)
z.test(x, mu = mu0, sd = sigma, alternative = "twop.sided", 
       conf.level = 1-alpha)
# One Sample z-test
# data:  x
# z = 0.99411, n = 25, Std. Dev. = 5, Std. Dev. of the sample mean = 1, p-value = 0.3202
# alternative hypothesis: true mean is not equal to 99
# 95 percent confidence interval:
#  98.03415 101.95408
# sample estimates:
# mean of x 
# 99.99411 

# Mention that value z in the output of z.test() denotes the value of test.stat
z.test(x, mu = mu0, sd = sigma, alternative = "two.sided", conf.level = 1-alpha)$statistic
test.stat

# probability beta of a type II error if mu1 = 103
# Solution
# Write the decision rule in terms of mv, i.e. reject H0 if mv < l or mv > u.
# You will not reject H0 if l <=  mv <= u.
# mv ~ N(mu1,sigma^2/n).
mu1 <- 103
l <- mu0 - qnorm(1-alpha/2)*sigma/sqrt(n)
u <- mu0 + qnorm(1-alpha/2)*sigma/sqrt(n)
# reject if (mv < l) | (mv > u) 
beta <- pnorm(u, mean = mu1, sd = sigma/sqrt(n)) - pnorm(l, mean = mu1, sd = sigma/sqrt(n)) 
beta # 0.02067337

# Determine the minimum sample size that a deviation of +-delta from mu0 will be detected
# with a probability of at most 1-beta
delta <- 3
beta <- 0.02
(sigma*(qnorm(1-alpha/2)+qnorm(1-beta))/delta)^2


##########################################################################
# (b) expected value, variance unknown
##########################################################################
# direct calculation
s <- sd(x)
q <- qt(1-alpha/2, df = n-1)
mv <- mean(x)
# confidence interval [l; u]
l <- mv - q * s / sqrt(n)
u <- mv + q * s / sqrt(n)
l; u # 98.04349; 101.9447

# test H0: mu = 99
mu0 <- 99
test.stat <- (mv - mu0)*sqrt(n)/s
# reject H0, if test.stat < -qt(1-alpha/2,n-1) or test.stat > qt(1-alpha/2,n-1)
(test.stat < - qt(1-alpha/2, df = n-1)) | 
  (test.stat > qt(1-alpha/2, df = n-1))
# false -> no rejection
# pvalue
pval <- 1-(pt(abs(test.stat), df = n-1)-pt(-abs(test.stat), df = n-1))
pval #  0.3033465

# test and confidence interval using t.test
t.test(x, mu = mu0, alternative = "two.sided", conf.level = 1-alpha)
# One Sample t-test
# data:  x
# t = 1.0518, df = 24, p-value = 0.3033
# alternative hypothesis: true mean is not equal to 99
# 95 percent confidence interval:
# 98.04349 101.94473
# sample estimates:
# mean of x 
# 99.99411 

# probability beta of a type II error if mu1 = 103
# Solution
# Write the decision rule in terms of test.stat, i.e. reject H0
# if test.stat < -qt(1-alpha/2,df=n-1) or test.stat > qt(1-alpha/2,df=n-1)
# You will not reject H0 if -qt(1-alpha/2,df=n-1) <= test.stat <= qt(1-alpha/2,df=n-1).
# <=> -qt(1-alpha/2,df=n-1)+(mu0-mu1)*sqrt(n)/ s <= (mv-mu1)*sqrt(n)/s <=
# +qt(1-alpha/2,df=n-1)+(mu0-mu1)*sqrt(n)/ s
# (mv-mu1)*sqrt(n)/s ~ t with df = n-1
# mention that the lower and upper bounds depend on the sample values 
# Assumption s = sd(x)
mu1 <- 103
beta <- pt(qt(1-alpha/2,df=n-1)+(mu0-mu1)*n^0.5/sd(x), df=n-1)-
  pt(-qt(1-alpha/2,df=n-1)+(mu0-mu1)*n^0.5/sd(x), df=n-1)
beta # 8.577371e-22

##########################################################################
# (c) variance, expected value unknown
##########################################################################
s <- sd(x)
q1 <- qchisq(1-alpha/2, df = n-1)
q2 <- qchisq(alpha/2, df = n-1)
# confidence interval [l; u]
l <- (n-1) * s^2 / q1
u <- (n-1) * s^2 / q2
l; u # 13.61506; 43.2173

# test H0: sigma = 3
sigma0 <- 3
test.stat <- (n-1) * s^2 / sigma0^2
# reject H0, if test.stat < qchisq(alpha/2,n-1) or test.stat > qchisq(1-alpha/2,n-1)
(test.stat < qchisq(alpha/2, df = n-1)) | 
  (test.stat > qchisq(1-alpha/2, df = n-1))
# true -> rejection
# pvalue
pval <- 2 * min(pchisq(test.stat, df = n-1),1-pchisq(test.stat, df = n-1))
pval # 0.0001478671

# test and confidence interval using sigma.test from the TeachingDemos library
library(TeachingDemos)
sigma.test(x, sigma = sigma0, alternative = "two.sided", 
           conf.level = 1-alpha)
# One sample Chi-squared test for variance
# data:  x
# X-squared = 59.549, df = 24, p-value = 0.0001479
# alternative hypothesis: true variance is not equal to 9
# 95 percent confidence interval:
# 13.61506 43.21730
# sample estimates:
# var of x 
# 22.33101 

# probability beta of a type II error if sigma = 4
# Mention that a type II error occurs if H0 is true and we do not reject H0.
# Solution:  
# Write the decision rule in terms of s^2, i.e. reject H0 if s^2 < l or s^2 > u.
# You will not reject H0 if l <= s^2 <= u.
# (n-1)s^2/sigma^2 ~ chi^2_(n-1)
sigma1 <- 4
l <- sigma0^2*qchisq(alpha/2,df=n-1)/(n-1)
u <- sigma0^2*qchisq(1-alpha/2,df=n-1)/(n-1)
# reject if (s^2 < l) | (s^2 > u), i.e. 
# (s^2 *(n-1)/sigma1^2 < l*(n-1)/sigma1^2) | 
# (s^2*(n-1)/sigma1^2 > u*(n-1)/sigma1^2)
beta <- pchisq(u*(n-1)/sigma1^2, df=n-1) - pchisq(l*(n-1)/sigma1^2, df=n-1)
beta #  0.4289457


##########################################################################
# Tests, confidence interval in case of binomial distributions
##########################################################################
set.seed(4711)

# generate a random sample from a B(n,p)-distribution
p <- 0.2; size <- 1; n <- 200
x <- rbinom(n,size,p)
x

# approximate test and confidence interval [l,u]
alpha <- 0.05
phat <- sum(x)/n
phat
l <- phat - qnorm(1-alpha/2) * sqrt(phat * (1-phat)/n)
u <- phat + qnorm(1-alpha/2) * sqrt(phat * (1-phat)/n)
l;u # 0.1400904; 0.2499096

# test H0: p = 0.15
p0 <- 0.15
test.stat <- (phat - p0) / sqrt(p0 * (1-p0) / n)
test.stat
# reject if test.stat < -qnorm(1-alpha/2) or test.stat > qnorm(1-alpha/2)
(test.stat < -qnorm(1-alpha/2) ) | 
  (test.stat > qnorm(1-alpha/2))
# false -> no rejection
# p-value
pval <- 1-(pnorm(abs(test.stat)) - pnorm(-abs(test.stat)))
pval #  0.07470593

library(binom)
binom.confint(x=sum(x), n=length(x),conf.level = 1-alpha, methods = "asymptotic")


# exact test and confidence from binom.test()
binom.test(sum(x), n , p = p0, alternative = "two.sided", conf.level = 1-alpha)
# Exact binomial test
# data:  sum(x) and n
# number of successes = 39, number of trials = 200, p-value = 0.0913
# alternative hypothesis: true probability of success is not equal to 0.15
# 95 percent confidence interval:
# 0.1424919 0.2567857
# sample estimates:
# probability of success 
# 0.195 

# exact pvalue: H0: X ~ B(n,p0)
# pvalue = P(X >= np + |sum(x)-pn|)+P(X <= np - |sum(x)-pn|)
1-pbinom(sum(x)-1, size = n, prob = p0)+pbinom(2*n*p0-sum(x), size = n, prob = p0)

# probability beta of a type II error if p1 = 0.22
# Mention that a type II error occurs if H0 is true and we do not reject H0.
# Solution  
# Write the decision rule in terms of X = sum(x), i.e. reject H0 if X<l or X>u.
# You will not reject H0 if l <= X <= u
# X ~ N(n p_1,n P-1 (1-p_1)) approximately.
p1 <- 0.22
l <- n*(p0-qnorm(1-alpha/2)*sqrt(p0*(1-p0)/n))
u <- n*(p0+qnorm(1-alpha/2)*sqrt(p0*(1-p0)/n))
# reject if (Y < l) | (Y > u) 
beta <- pnorm(u+0.5, mean = n*p1, sd=sqrt(n*p1*(1-p1))) - 
  pnorm(l+0.5, mean = n*p1, sd=sqrt(n*p1*(1-p1))) # continuity correction!!
beta #  0.2692564

# exact value
l.ex <- qbinom(alpha/2, size = n, prob = p0)
u.ex <- qbinom(1-alpha/2, size = n, prob = p0)
beta_exact <- pbinom(u.ex, size = n, prob = p1)-
  pbinom(l.ex-1, size = n, prob = p1)
beta_exact