###################################################################
# Suppose that two fair, standard dice are tossed and the sequence
# of scores (X_1,X_2) are recorded. Let Y=X_1+X_2, denote the sum 
# of the scores, U=min (X_1,X_2), the minimum score, and 
# V=max (X_1,X_2) the maximum score.
#
# file: prob_rv_rolling_dice_sol.R
###################################################################

library(tidyverse)

#################################################################
# solution without gtools 
#################################################################
# a) Find the probability density function of (X_1,X_2).
# expand.grid() create a data frame from all combinations of the 
# supplied vectors or factors.
x1_x2_dens <- tibble(
  x1 = rep(1:6, length.out=6^2),
  x2 = rep(1:6, each=6, length.out=6^2),
  prob = 1/6^2
)
x1_x2_dens

# b) Find the probability density function of Y.
x1_x2_dens %>%
  mutate(y = x1+x2) %>%
  count(y) %>%
  mutate(prob = n/36) %>%
  select(-n) -> y_dens
y_dens

# c) Find the probability density function of U.
# apply() returns a vector or array or list of values obtained by 
# applying a function to margins of an array or matrix.
x1_x2_dens %>%
  rowwise() %>%
  mutate(u = min(x1,x2)) %>%
  count(u) %>%
  mutate(prob = n/36) %>%
  select(-n) -> u_dens
u_dens

# d) Find the probability density function of V.
x1_x2_dens %>%
  rowwise() %>%
  mutate(v = max(x1,x2)) %>%
  count(v) %>%
  mutate(prob = n/36) %>%
  select(-n) -> v_dens
v_dens

# e) Find the probability density function of (U,V).
x1_x2_dens %>%
  rowwise() %>%
  mutate(
    u = min(x1,x2),
    v = max(x1,x2)
  ) %>%
  count(u,v) %>%
  mutate(prob = n/36) %>%
  select(-n) -> uv_dens
uv_dens