##################################################
# An advertising agency wants to construct a 99% 
# confidence lower bound for the proportion of 
# dentists who recommend a certain brand of toothpaste. 
# The margin of error is to be 0.02. How large should
# the sample be?
#
# file: infstat_conf_interval_prop_sample_size.R
##################################################

alpha <- 0.01; margin <- 0.02
c <- qnorm(1-alpha,0,1)
f <- seq(0,1,length=101)
n <- max(ceiling(c^2 * f*(1-f)/(margin^2)))
n

# If f <= 0.2, we get
f <- seq(0,0.2,length=21)
n <- max(ceiling(c^2 * f*(1-f)/(margin^2)))
n

