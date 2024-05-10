########################################################
# You read about a survey in a newspaper and find 
# that 70% of the 250 people sampled prefer Candidate A.
# a) Compute the 95% confidence interval.
# b) You are surprised by this survey because you thought 
#    that more like 50% of the population preferred this 
#    candidate. Based on this sample, is 50% a possible 
#    population proportion?
#
# file: infstat_conf_interval_prop_survey.R
########################################################

n <- 250; p <- 0.7; alpha <- 0.05

# normal approximation
l.appr <- p - qnorm(1-alpha/2)*sqrt(p*(1-p)/n)
u.appr <- p + qnorm(1-alpha/2)*sqrt(p*(1-p)/n)
l.appr; u.appr

# exact
xp <- seq(0,1,length=1+10^4)
l.ex <- xp[min(which(qbinom(1-alpha/2,n,xp) == p*n))]
u.ex <- xp[max(which(qbinom(alpha/2,n,xp) == p*n))]
l.ex; u.ex

# exact confidence interval with R-function
binom.test(x=0.7*250,n=250,conf.level=1-alpha)$conf.int