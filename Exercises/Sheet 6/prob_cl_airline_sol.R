##############################################################
# An airline knows that over the long run, 90% of  passengers
# who reserve seats show up for their flight. On a particular 
# flight with 300 seats, the airline accepts 324 reservations.
#
# file: prob_cl_airline_sol.R
##############################################################

# a) Assuming that passengers show up independently of each other,
# what is the chance that a passenger with a reservation do not 
# get a seat?
n <- 324; p <- 0.9
# exact value
p_ex <- 1-pbinom(300.5,n,p)
p_ex
# approx. value
m <- n*p; s <- sqrt(n*p*(1-p))
p_app <- 1-pnorm(300.5,m,s)
p_app

# b) How many reservations can be given, if the airline will
# accept an overbooking probability of 1%?

# exact bound
n <- seq(301,350,1)
p_ex <- pbinom(300,n,p)
o_ex <- n[max(which(p_ex >= 0.99))]
o_ex

# approx. bound
m <- n*p; s <- sqrt(n*p*(1-p))
p_app <- pnorm(300.5,m,s)
o_app <- n[max(which(p_app >= 0.99))]
o_app

# alternative solution: solving the equation
# (300.5 - p*n)^2 = p*(1-p)*u^2_0.99*n
# quadratic equation: n^2 + a*n +b =0
u_099 <- qnorm(0.99,0,1)
#u_099
#p
a <- -(2*p*300.5 + p*(1-p)*u_099^2)/(p^2)
b <- (300.5/p)^2
# a;b
n1 <- -a/2 + sqrt((a/2)^2 -b)
n2 <- -a/2 - sqrt((a/2)^2 -b)
n1; n2 
300.5-n1*p 
300.5-n2*p 

# solution without solving the above equation
library(tidyverse)
tibble(
  n = 300:324,
  p = 1-pbinom(300, size = n, prob = 0.9),
  p.app = 1-pnorm(300.5, mean = n*0.6, sd = sqrt(n*0.9*0.1))
) %>% filter(p >= 0.01) %>% filter(n == min(n)) # n < 321!!