####################################################
# At a telemarketing firm, the length of a telephone 
# solicitation (in seconds) is a normally distributed 
# random variable with mean mu and standard deviation
# sigma, both unknown. A sample of 50 calls has mean 
# length 300 and standard deviation 60.
#
# file: infstat_conf_interval_telefirm.R
#####################################################
n <- 50; m <- 300; s_sample <- 60; alpha <- 0.05

#  a) Construct the 95% confidence upper bound for mu.
t_a <- qt(1-alpha,n-1)
t_a
o <- m+t_a*s_sample/sqrt(n)
o

#  b) Construct the 95% confidence lower bound for sigma.
chi <- qchisq(1-alpha,n-1)
chi
u <- (n-1)*s_sample^2/chi
sqrt(u)