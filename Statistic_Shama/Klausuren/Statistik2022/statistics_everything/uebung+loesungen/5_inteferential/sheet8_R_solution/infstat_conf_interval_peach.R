########################################################
# At a certain farm the weight of a peach (in ounces) 
# at harvest time is a normally distributed random 
# variable with standard deviation 0.5. How many peaches 
# must be sampled to estimate the mean weight with a
# margin of error pm 0.2 and with 95% confidence.
#
# file: infstat_conf_interval_peach.R
########################################################

alpha <- 0.05; s <- 0.5; margin <- 0.2
q_a <- qnorm(1-alpha/2,0,1); q_a
n <- ceiling((q_a*s/margin)^2)
n