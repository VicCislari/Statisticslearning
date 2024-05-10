################################################################
# A vaccine that is currently used to immunize people against a 
# certain infection has an 80% success rate. That is, 80% of 
# individuals who receive this vaccine will develop immunity 
# against the infection. A manufacturer of a new vaccine claims 
# that its vaccine has a higher success rate.
# file: infstat_testing_vaccine.R
################################################################

# a) parameter of interest: success rate p

# b) Suppose in a clinical trial, 200 people received the new
# vaccine. Of these, 172 became immune to the infection. Based on
# this, can we say that the new vaccine is indeed more effective
# than the current one? 
# What is the corresponding Null-Hypothesis?
# Null Hypothesis H0: p<=p0   against H1: p>p0
p0 <- 0.8
# Test at a 5% significance level and state your conclusion in the
# context of the problem. 
n <- 200
p <- 172/n
alpha <- 0.05
# normal approximation
test_statistic <- (p-p0)/sqrt(p0*(1-p0)/n)
# reject if test_statistic > qnorm(1-alpha)
test_statistic > qnorm(1-alpha) # 2.12132 > 1.644854
# reject H0, thus the new vaccine seems to be better than the old one
p_value_app <- 1-pnorm(test_statistic) # 0.01694743
# exact test
binom.test(172,p=0.8,n,alternative = "greater", conf.level = 1-alpha)
p_value <- 1-pbinom(n*p-1,n,p0)
p_value # = 0.01792922 < 0.05, i.e. rejection of H0

# c) In making the above conclusion, which type of error are you
# risking, type I or type II?
# Since we reject H0, we are risking making type I error.

# d) What is the probability of a type II error if the true success
# rate is 82%? 
p1 <- 0.82
# beta.approx = P(test_statistic <= qnorm(1-alpa)) if p=p1 <=>
# beta.approx = P(p <= qnorm(1-alpa)*(p0(1-p0)/n)^0.5+p0)
# with p ~ N(p1,p1(1-p1)/n) approx.
beta.approx <- pnorm(qnorm(1-alpha)*(p0*(1-p0)/n)^0.5+p0,
                     mean = p1, sd = (p1*(1-p1)/n)^0.5)
beta.approx
# alternative beta.approx =
pnorm(qnorm(0.95, mean = 0.8, sd = (0.8*0.2/n)^0.5),
      mean = p1, sd = (p1*(1-p1)/n)^0.5)

# beta.exact = P(np <= 95% quantile of B(n,p0) with n*p ~ B(n,p1) 
beta_exact <- pbinom(qbinom(1-alpha,size = n, prob = p0),
                     size = n, prob = p1)
beta_exact

# e) What should be the minimal sample size if the probability
# of the type II error should be less than 5%?
library(tidyverse)
tibble(
  n = 200:5000,
  b.ex = pbinom(qbinom(1-alpha,size = n, prob = p0),
                size = n, prob = p1),
  b.approx = pnorm(qnorm(1-alpha)*(p0*(1-p0)/n)^0.5+p0,
                   mean = p1, sd = (p1*(1-p1)/n)^0.5)
) %>% filter(b.approx <= 0.05) %>% filter(n == min(n))

# direct determination of n using a normal approximation
(qnorm(0.95)*(sqrt(p0*(1-p0))+sqrt(p1*(1-p1)))/(p1-p0))^2
