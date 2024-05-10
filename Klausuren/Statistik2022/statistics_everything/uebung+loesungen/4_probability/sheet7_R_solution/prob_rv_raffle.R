##########################################################
# A company organizes a raffle at an end-of-year function.
# There are 4000 raffle tickets to be sold, of which 500 
# win a prize. The price of each ticket is 1.5 Euro. The 
# value of the prizes varies between 80 Euro and 250 Euro 
# with an average of 142 Euro.
#
# file: prob_rv_raffle.R
###########################################################

library(tidyverse)

# a) An employee wants to have a 99% guarantee of receiving 
# three prizes. How much money does he need to spend?

# X number of drawn prize tickets, X ~ H(n,M=500,N=4000)
# with n number of drawn tickets
dis <-
  tibble(
    n=seq(1,4000),
    p=1-phyper(2,500,3500,n) # prob. of at least 3 wins
  )
dis
# 99% quantile
min(which(dis$p>=0.99)) 
# or
dis %>%
  filter(p >= 0.99) %>%
  summarise(money = min(n))

# b) Use R to plot the function which describes the relationship
# between the number of tickets bought and the probability of 
# winning at least three prizes.
plot(x=dis$n,y=dis$p,
     type = "l", xlim = c(0,75),
     xlab = "n", ylab = "p")
abline(a=0.99, b=0)
