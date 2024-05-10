##############################################################################
# Exercise: Bayes and Binomial Distribution
# Consider the following random experiment: A fair die is rolled. Then, a fair 
# coin is flipped many times according to the number of the die's points. The 
# number of times heads occur is counted with the random variable Score.
#
# file: prob_die_coin_sol.R
##############################################################################  

library(tidyverse)

# I: fair die rolled -> score
# II: flip a fair coin score times and counts the number of heads

# a) Determine the density of the random variable score. What are the expected 
#    value and variance of Score?
tibble(
  score = rep(1:6, each=7),
  heads = rep(0:6, length.out = 6*7)
) %>%
  group_by(score,heads) %>%
  mutate(
    prob.heads.given.score = sum(dbinom(heads, size=score, prob=0.5)),
    prob.score = 1/6,
    prob.heads.and.score = prob.heads.given.score * prob.score
  ) %>%
  group_by(heads) %>%
  mutate(prob.heads = sum(prob.heads.and.score),
         prob.score.given.heads = prob.heads.and.score/prob.heads
  ) -> ds 

ds %>%
  select(heads, prob.heads) %>%
  unique() -> density.heads

E.X <- sum(density.heads$heads*density.heads$prob.heads) # 1.75
E.X2 <- sum(density.heads$heads^2*density.heads$prob.heads)
var.X <- E.X2-E.X^2 # 1.60417

# Alternative Bestimmung der Dichte der Anzahl Kopf
density.heads <- tibble(x=0:6) %>%
  # rowwise evaluation!!!!!
  rowwise() %>% 
  mutate(
    # size >= 1!!!
    dens.heads = sum(dbinom(x,size=max(1,x):6, prob = 0.5))/6
  )

# b) Let only the number of heads be known. What are the probabilities for the 
#    values of the score with respect to the different numbers of heads?
# P(Score = s | heads = h) > 0 if s >= h!!!
bayes <- ds %>%
  select(heads,score,prob.score.given.heads) %>%
  arrange(heads, score) %>%
  filter(prob.score.given.heads>0)