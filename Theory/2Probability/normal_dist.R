########################################################
# Normal Distributions
#
# file: normal_dist.R
########################################################

# Example 1
# Assume the speed of vehicles along a stretch of a
# highway has an approximately normal distribution with a
# mean of 71 mph and a standard deviation of 8 mph.
mu <- 71; sigma <- 8

# 1. The current speed limit is 65 mph. What is the
# proportion of vehicles less than or equal to the speed
# limit?
pnorm(65, mean = mu, sd = sigma)

# 2. What proportion of the vehicles would be going more
# than 70 mph?
1-pnorm(70, mean = mu, sd = sigma)

#   3. What proportion of the vehicles would be going less
# than 70 mph and more than 50 mph?
pnorm(70, mean = mu, sd = sigma) - pnorm(50, mean = mu, sd = sigma)

#   4. A new speed limit will be initiated such that
# approximately 10% of vehicles will be over the speed
# limit. What is the new speed limit based on this
# criterion?
qnorm(0.9, mean = mu, sd = sigma)
#   5. In what way do you think the actual distribution of
# speeds differs from a normal distribution?

####################################################################
# Example 2
# Online Statistics VII Exercise 5: Questionnaire to assess
# women’s and men’s attitudes toward using animals in
# research.
# One question: whether animal research is wrong to
# be answered on a 7-point scale.
# Assumption: mean for women = 5, mean for men = 4, standard deviation for both 
# groups = 1.5, scores normally distributed
mu.m <- 4; mu.f <- 5; sigma <- 1.5

# If 12 women and 12 men are selected randomly, what is
# the probability that the mean of the women will be more
# than 1.5 points higher than the mean of the men?
n <- 12
1-pnorm(1.5, mean = mu.f-mu.m, sd = (2*sigma^2/n)^0.5)

####################################################################
# Example 3
# Volume of a bottle of is normaly distributed N(0.5, 0.01^2)
mu <- 0.5; sigma <- 0.01

# Probability, that a bottle contains at least 0.48l
1-pnorm(0.48, mean = mu, sd = sigma)

# Minimum volume, which is observed with a probability of 95%
qnorm(0.05, mean = mu, sd = sigma)

# Range symmetrical to the mean value, which contains the volume with
# a probability of 95%.
qnorm(c(0.025,0.975), mean = mu, sd = sigma)

# Minimum volume sufficient to fill 5000 bottles with a probability of 95%.
qnorm(0.05, mean = 5000*mu, sd = (5000*sigma^2)^0.5)

# Example 4: Central Limit Theorem
# 100 rolls of a fair die: probability of at most 20 times 6
p <- 1/6; n <- 100
# exact value
pbinom(20, size = n, prob = p) 
# normal approximation
pnorm(20, mean = n*p, sd = (n*p*(1-p))^0.5)
# normal approximation with continuity correction
pnorm(20+0.5, mean = n*p, sd = (n*p*(1-p))^0.5)
  
# Example 5: Central Limit Theorem
# A town is planing a housing estate with 40 flats. To know how many 
# playgrounds, sportfields, ... are needed, the administration needs 
# informations about the numbers of children in the new housing estate. 
# If the probability that more than 40 children will be in the new 
# housing estate is less than 10% the administration can continue with 
# planing.
# Last sample census
children.household <- c(0,1,2,3,4)
ratio <- c(0.52,0.24,0.18,0.05,0.01)
# Ratio of households with more than 4 children is negligible!

# X.i = # children in household i=1,..,40
EX.i <- sum(children.household*ratio)
EX2.i <- sum(children.household^2*ratio)
VarX.i <- EX2.i-(EX.i)^2
# C = X.1+...+X.40 ~ N(40*EX.i, 40*VarX.i) approximately
# P(C > 40)
1-pnorm(40, mean = 40*EX.i, sd = (40*VarX.i)^0.5)
# with continuity correction
1-pnorm(40+0.5, mean = 40*EX.i, sd = (40*VarX.i)^0.5)
# 90% quantil
qnorm(0.9, mean = 40*EX.i, sd = (40*VarX.i)^0.5)

