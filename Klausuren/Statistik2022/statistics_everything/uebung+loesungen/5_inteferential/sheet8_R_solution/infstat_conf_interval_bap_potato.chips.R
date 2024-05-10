##################################################
# Suppose that the weight of a bag of potato chips 
# (in grams) is a normally distributed random  
# variable with mean mu and standard deviation sigma, 
# both unknown. A sample of 75 bags has mean 250
# and standard deviation 10.
#
# file: infstat_conf_interval_bap_potato.chips.R
##################################################

n <- 81; m <- 250; s_sample <- 10; alpha <- 0.1

#  a) Construct the 90% confidence interval for mu.
t_a <- qt(1-alpha/2,n-1)
t_a
u <- m-t_a*s_sample/sqrt(n)
o <- m+t_a*s_sample/sqrt(n)
u;o

#  b) Construct the 90% confidence interval for sigma.
chi_1 <- qchisq(alpha/2,n-1)
chi_2 <- qchisq(1-alpha/2,n-1)
chi_1; chi_2
o <- (n-1)*s_sample^2/chi_1
u <- (n-1)*s_sample^2/chi_2
sqrt(u); sqrt(o)

# c) Construct the above confidence interval for mu, 
# if sigma = 10 is the standard deviation of the
# population.
q_a <- qnorm(1-alpha/2,0,1)
q_a
u <- m-q_a*s_sample/sqrt(n)
o <- m+q_a*s_sample/sqrt(n)
u;o
