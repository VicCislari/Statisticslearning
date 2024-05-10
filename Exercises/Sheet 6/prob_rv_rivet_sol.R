####################################################################
# Consider a lottery of 20 tickets. Among the tickets there 
# are a first prize, 4 second prizes and 15 rivets. 5 tickets
# are drawn from the lottery drum. Determine the probability that
# a) 2 rivets have been drawn.
# b) 2 rivets, 2 second prizes and the first prize were drawn.
# c) The 5th lot drawn is the first lot which is not a rivet.
# Calculate the probabilities if the lots are drawn with replacement.
# What results will be obtained if the number of lots in the drum is
# increased (with equal proportions of first prizes, second prizes
# and rivets)?
#
# file: prob_rv_rivet_sol.R
#####################################################################

library(tidyverse)

fp <- 1
sp <- 4
riv <- 15
n <- 5

# without replacement
# a) hypergeometric distribution
# H(k, M, N+M) density:
#dhyper(x = number of white balls drawn, 
#       m = number of white balls in the urn, 
#       n	= number of black balls in the urn, 
#       k	= the number of balls drawn from the urn)
pa <- dhyper(2,riv,fp+sp,n)
pa # = choose(15,2)*choose(5,3) / choose(20,5)

# b) generalised hypergeometric distribution
pb <- choose(riv,2)*choose(sp,2)*choose(fp,1) /
  choose(riv+fp+sp,n)
pb
# c) geometric distribution
pc <- (choose(riv,n-1)/choose(fp+sp+riv,n-1)) *
  (fp+sp)/(fp+sp+riv-(n-1))
pc

# results with and without replacement for increasing
# number of tickets but constant proportion of the
# prizes
results <-
  tibble(
    m = seq(from=1,to=55,by=5),
    fp = m,
    sp = 4*m,
    riv = 15*m,
    pa_worepl = dhyper(2,riv,fp+sp,5),
    pa_wrepl = dbinom(2,5,riv/(riv+fp+sp)),
    pb_worepl = choose(riv,2)*choose(sp,2)*choose(fp,1) /
      choose(riv+fp+sp,1+2+2),
    pb_wrepl = factorial(1+2+2)/
      (factorial(2)*factorial(2)*factorial(1))*
      (riv/(riv+fp+sp))^2 * (sp/(riv+fp+sp))^2 *
      (fp/(riv+fp+sp))^1,
    pc_worepl = (choose(riv,5-1)/choose(fp+sp+riv,5-1)) *
      (fp+sp)/(fp+sp+riv-(5-1)),
    pc_wrepl = dgeom(5-1,(fp+sp)/(fp+sp+riv))
  )
results
