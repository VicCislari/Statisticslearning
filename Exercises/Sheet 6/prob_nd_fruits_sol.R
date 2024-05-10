#################################################################
# The weight of a melon, X, in kg is N(mu=1.2,sigma^2=0.3^2), 
# i.e. normally distributed with expectation 1.2 kg and standard
# deviation 0.3 kg. The weight Y for a pineapple is in kg 
# N(mu=0.6, sigma^2=0.2^2). We assume that a melon and a pineapple
# are chosen independently of each other. 
#
# file: prob_nd_fruits_sol.R
##################################################################

mu_m <- 1.2
sigma_m <- 0.3
mu_p <- 0.6
sigma_p <- 0.2

# a) Which distribution has the total weight of the two fruits? 
# S = X + Y
mu_s <- mu_m + mu_p
sigma_s <- (sigma_m^2+sigma_p^2)^0.5

# b) Calculate the probability that the total weight of the two fruits 
# does not exceed 2.0 kg.  
pnorm(2,mu_s,sigma_s)

# c) The melon costs 2 euro per kg and the pineapple 4 euro per kg. 
# Give an expression for the total price Z using X and Y. What is the 
# distribution of Z?  
mu_z <- 2*mu_m + 4*mu_p
sigma_z <- (2^2*sigma_m^2 + 4^2*sigma_p^2)^0.5

# d) Calculate the probability that the price Z is higher than 4 euro.
1-pnorm(4,mu_z,sigma_z)
