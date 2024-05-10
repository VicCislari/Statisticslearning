##################################################################
# A machine consists of the three modules A, B and C. The machine
# works only if all three modules are working and if no error
# occured during the construction phase. The probabilities that 
# the modules A, B and C are defect are 1%, 1% and 5%. The 
# probability for an error during the construction phase 2%. The
# four kinds of errors occur independently of each other. 
#
# file: prob_cl_modules_sol.R
##################################################################

# a) Calculate the expectation and the variation of the number of 
# defect machines in a lot of 1000 randomly chosen machines. 
n <- 1000
pa <- 0.01
pb <- 0.01
pc <- 0.05
pcp <- 0.02
# prob. of no error
p_no_error <- (1-pa)*(1-pb)*(1-pc)*(1-pcp)
# expected value
exp_n_def <- n*(1-p_no_error)
# variance
var_n_def <- n*(1-p_no_error)*p_no_error
p_no_error; exp_n_def; var_n_def

# b) The producer is thinking about guaranteeing that not more 
# than 110 machines are defect in such a lot. With which approximate 
# probability can this guarantee promise be kept?  
pbinom(110,n,1-p_no_error) # 0.9936782
# approximation by a normal distribution
# without continuity correction
pnorm(110,mean=exp_n_def,sd=(var_n_def)^0.5) # 0.9940429
# with continuity correction
pnorm(110.5,mean=exp_n_def,sd=(var_n_def)^0.5) # 0.9949242

# c) Each defect machine provokes an extra cost of 100 euro. 
# The producer considers to buy a better module C (at a higher
# price) but with an error rate of is 1%. How high can the 
# additional cost for each machine for module C be, in order to
# say that it is (according to the expectation) profitable to buy
# the more expensive module C?
p_no_error_new <- (1-pa)*(1-pb)*(1-0.01)*(1-pcp)
100*(1-p_no_error) - 100*(1-p_no_error_new)
