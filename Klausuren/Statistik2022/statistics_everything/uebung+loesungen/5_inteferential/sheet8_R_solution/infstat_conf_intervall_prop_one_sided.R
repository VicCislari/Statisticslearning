##################################################
# A researcher was interested in knowing how many 
# people in the city supported a new tax. She sampled
# 100 people from the city and found that 40% of 
# these people supported the tax. What is the upper
# limit of the 95% (one-side) confidence interval 
# on the population proportion?
#
# file: infstat_conf_intervall_prop_one_sided.R
##################################################

n <- 100; p <- 0.4; alpha <- 0.05

# normal approximation
u.appr <- p + qnorm(1-alpha)*sqrt(p*(1-p)/n)
u.appr

# exact
xp <- seq(0,1,length=1+10^4)
u.ex <- xp[max(which(qbinom(alpha,n,xp) == p*n))]
u.ex

# exact confidence interval with R-function
binom.test(x=40, n=100, alternative = "less",
           conf.level=1-alpha)$conf.int