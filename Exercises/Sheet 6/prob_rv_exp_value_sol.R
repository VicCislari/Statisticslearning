######################################################
# In a game a player can bet 1$ on any of the numbers 
# 1, 2, 3, 4, 5 and 6. Three dice are rolled. If the 
# players number appears k times, where k >= 1, the player
# gets k$ back plus the original stack of 1$. Over the 
# long run, how many cents per game a player expects to 
# win or lose playing this game?
#
# file: prob_rv_exp_value_sol.R
#######################################################

library(tidyverse)

# create a tibble with the different outcomes and their
# probabilities
R <- tibble(
    k=0:3,
    prob=dbinom(k, size = 3, prob = 1/6), 
    #prob=choose(3,k)*(1/6)**k*(5/6)**(3-k),
    r = if_else(k==0,-1,-1+1+k))
R
# expected value
exp_r <- sum(R[,2]*R[,3])
exp_r 

