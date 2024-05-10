###################################################################
# A coin is tossed 500 times and results in 302 
# heads. At the 0.05 level, test to see if the 
# coin is unfair.
#
# file: infstat_testing_coin_tosses.R
####################################################################
n <- 500
h <- 302
p0 <- 0.5
p <- h/n
alpha <- 0.05

test_statistic <- (p-p0)/(p0*(1-p0)/n)^0.5
test_statistic # 4.651021

lb_rejection_region <- -qnorm(1-alpha/2,0,1)
ub_rejection_region <- qnorm(1-alpha/2,0,1)
lb_rejection_region; ub_rejection_region
# -1.959964; 1.959964

test_statistic < lb_rejection_region ||
  test_statistic > ub_rejection_region
# TRUE -> reject H0

# pvalue: approximate test
2*pnorm(-abs(test_statistic))

# exact test
binom.test(x = c(h,n-h), alternative = "two.sided", conf.level = 1-alpha)

# pvalue: exact test
pbinom(n-h, size = n, prob = 0.5) + (1-pbinom(h-1, size = n, prob = 0.5))