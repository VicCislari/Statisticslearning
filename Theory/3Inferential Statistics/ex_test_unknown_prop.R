###########################################################
# Heumann, Schomaker, p 228
# test on an unknown proportion
#
# file: ex_test_unknown_prop.r
###########################################################
n <- 2000
alpha <- 0.05
x <- 700
p.hat <- x/n

# H0: p <= 0.3
p0 <- 0.3

# approximating test
# test statistics
test.stat <- (p.hat-p0)*sqrt(n)/sqrt(p0*(1-p0))
# decision rule
test.stat > qnorm(1-alpha)
# rejection
p.value <- 1-pnorm(test.stat)
p.value
# approximate lower confidence bound
l <- p.hat - qnorm(1-alpha)*sqrt(p.hat*(1-p.hat)/n)
p0 >= l

# exact test: reject H0, if X> 95% quantil of B(n,p0)
x > qbinom(1-alpha, size = n, prob = p0)
# rejection
p.value.exact <- 1-pbinom(x-1, size = n, prob = p0)
p.value.exact

# R command binom.test
binom.test(x=700, n=2000, p=0.3, alternative = "greater")
# Exact binomial test
# 
# data:  700 and 2000
# number of successes = 700, number of trials = 2000, p-value = 8.395e-07
# alternative hypothesis: true probability of success is greater than 0.3
# 95 percent confidence interval:
#   0.332378 1.000000
# sample estimates:
#   probability of success 
# 0.35 

# probability of a type II error if p=0.32
beta <- pbinom(qbinom(1-alpha, size = n, prob = p0),
               size = n, prob = 0.32)
beta 