##########################################################################
# Exercise: chi^2 goodness of fit test: discrete distribution
# 
# file: infstat_2samples_testing_height_mean.R
##########################################################################
library(tidyverse)
# The hypothesis is to be tested that the height of adult
# of adult German men is normally distributed (significance level
# 10%). For this purpose, a random sample is collected, which leads
# to the following findings: 
results <- tibble(
  from = seq(from=150, to=200, by=5),
  to = seq(from=155, to=205, by=5),
  no = c(20,30,55,60,85,80,50,40,30,15,10),
  mid = 0.5*(from+to)
)
results

# estimation of mean and sd
est.mean <- mean(rep(x=results$mid,times=results$no))
est.sd <- sd(rep(x=results$mid,times=results$no))

# expected number of observation
est.p <- pnorm(results$to, mean = est.mean, sd = est.sd) -
  pnorm(results$from, mean = est.mean, sd = est.sd)
est.no <- est.p * sum(results$no)

# H0: no normally distributed
chi2 <- sum((results$no-est.no)^2/est.no) 
# decision: reject H0, if chi^2 (1-alpha) quantile of chi^2 
# distribution with k = number of classes - 2 -1
chi2 > qchisq(1-0.1, df = length(results$from)-2-1)
1-pchisq(chi2,df = length(results$from)-2-1)
# reject H0, since p-value = 0.0598