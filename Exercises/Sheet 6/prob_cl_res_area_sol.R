###################################################################
# As a new residential area with 1000 domestic homes  is going 
# to be built, the number of required parking lots is calculated
# in the following way: We assume that there is no relation between
# the number of cars in different homes. Furthermore, we assume 
# that a domestic home has no car with probability 0.2, one car 
# with probability 0.7 and two cars with probability 0.1. The number
# of parking lots should be planned in such way that the probability
# that each car gets a parking lot is 0.99. How many parking lots 
# should be built?
#
# file: prob_cl_res_area_sol.R
###################################################################

p0 <- 0.2
p1 <- 0.7
p2 <- 0.1
n <- 1000
# expected values
EX <- 0*p0 + 1*p1 + 2*p2
EX2 <- 0^2*p0 + 1^2*p1 + 2^2*p2
# variance
VarX <- EX2 - (EX)^2
EX
VarX
# 99% quantile
qnorm(0.99,n*EX,(n*VarX)^0.5) # 939.6163