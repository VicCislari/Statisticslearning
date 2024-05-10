##############################################################
# Peter and Paul agree to meet at a restaurant at noon. Peter 
# arrives at a time normally distributed with mean 12:00 and 
# standard deviation 5 minutes. Paul arrives at a time normally
# distributed with mean 12:02 and standard deviation 2 minutes. 
# Assuming the two arrivals are independent, find the following
# probabilie.
#
# file: prob_nd_peter_paul.R
###############################################################
m1 <- 12; m2 <- 12+2/60
s1 <- 5/60; s2 <- 2/60

# a) Peter arrives before Paul
p <- pnorm(0, m1-m2, sqrt(s1^2+s2^2))
p 

# b) both men arrive within 3 minutes of noon
p <- (pnorm(12+3/60, m1, s1) - pnorm(12-3/60, m1, s1)) *
  (pnorm(12+3/60, m2, s2) - pnorm(12-3/60, m2, s2))
p

# c) the two men arrive within 3 minutes of each other
p <- pnorm(3/60, m1-m2, sqrt(s1^2+s2^2)) - pnorm(-3/60, m1-m2, sqrt(s1^2+s2^2))
p 
