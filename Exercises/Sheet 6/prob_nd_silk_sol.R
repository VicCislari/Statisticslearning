##################################################################
# In a silk spinning mill, raw fibers from silk cocoons are
# prepared to silk threads. It can be assumed that the useful silk
# thread length per cocoon is a normally distributed variable with
# expectation 800 m and variance 6400 m^2.  
#
# file: prob_nd_silk_sol.R
##################################################################

# Calculate the probability that the useful silk thread length
# from a randomly selected cocoon is at least 750 m. Also calculate
# the probability that the useful silk thread length from a randomly
# selected cocoon exceeds 1000 m. 
mu <- 800
sigma <- 80
pnorm(750,mu,sigma) 
1-pnorm(1000,mu,sigma) 

# Use appropriate assumptions and calculate the lower boundary
# L and the upper boundary U for the total length of the useful silk 
# thread for 10000 cocoons. These boundaries should at the same time 
# be guaranteed with a probability of 95%. The boundaries should be 
# selected in such a way that the probability for exceeding U and going 
# below L should be equally high. 
n <- 10000
L <- qnorm(0.025,n*mu,n^0.5*sigma)
U <- qnorm(0.975,n*mu,n^0.5*sigma)
L; U # 7984320, 8015680

# Assume that the variance still is the same as before. How high
# must the expetation of the useful silk thread length at least be, if
# we would like the total useful silk length to be at least 750 m with
# a probability of 0.90. 
mu_new <- 750-sigma*qnorm(0.1,0,1)
mu_new # 852.5241

# 10 cocoons are randomly chosen. With which probability is at
# most for one of these cocoons the useful silk thread length less
# than 750 m?  
p <- pnorm(750,mu,sigma)
pbinom(1,10,p) # 0.2099118