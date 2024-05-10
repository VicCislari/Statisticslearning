###################################################################
# Suppose that two fair, standard dice are tossed and the sequence
# of scores (X_1,X_2) are recorded. Let Y=X_1+X_2, denote the sum 
# of the scores, U=min (X_1,X_2), the minimum score, and 
# V=max (X_1,X_2) the maximum score.
#
# file: prob_rv_rolling_dice.R
###################################################################

library(gtools)
library(tidyverse)

# a) Find the probability density function of (X_1,X_2).
# expand.grid() create a data frame from all combinations of the 
# supplied vectors or factors.
density.X1.X2 <-
  permutations(n = 6,r = 2, v = 1:6, repeats.allowed = TRUE) %>% 
  as_tibble() %>%
  mutate(prob = 1/36) 

# b) Find the probability density function of Y.
density.Y <- density.X1.X2 %>%
  mutate(Y = V1+V2) %>%
  group_by(Y) %>%
  mutate(prob.Y = sum(prob)) %>%
  select(Y, prob.Y) %>%
  unique()

# c) Find the probability density function of U.
density.U <- density.X1.X2 %>%
  rowwise() %>%
  mutate(U = min(V1,V2)) %>%
  group_by(U) %>%
  mutate(prob.U = sum(prob)) %>%
  select(U, prob.U) %>%
  unique()

# d) Find the probability density function of V.
density.V <- density.X1.X2 %>%
  rowwise() %>%
  mutate(V = max(V1,V2)) %>%
  group_by(V) %>%
  mutate(prob.V = sum(prob)) %>%
  select(V, prob.V) %>%
  unique()

# e) Find the probability density function of (U,V).
density.UV <- density.X1.X2 %>%
  rowwise() %>%
  mutate(UV = paste(min(V1,V2),max(V1,V2))) %>%
  group_by(UV) %>%
  mutate(prob.UV = sum(prob)) %>%
  select(UV, prob.UV) %>%
  unique()

#################################################################
# alternative solution
#################################################################
# a) Find the probability density function of (X_1,X_2).
# expand.grid() create a data frame from all combinations of the 
# supplied vectors or factors.
x1 <- seq(1,6,1)
x2 <- seq(1,6,1)
x1_x2 <- expand.grid(x=x1,y=x2)
x1_x2_dens <- cbind(x1_x2,rep(1/36,36))
x1_x2_dens

# b) Find the probability density function of Y.
y <- x1_x2[,1]+x1_x2[,2]
y_dens <- table(y)/36
y_dens

# c) Find the probability density function of U.
# apply() returns a vector or array or list of values obtained by 
# applying a function to margins of an array or matrix.
u <- apply(x1_x2,1,min)
u_dens <- table(u)/36
u_dens

# d) Find the probability density function of V.
v <- apply(x1_x2,1,max)
v_dens <- table(v)/36
v_dens

# e) Find the probability density function of (U,V).
uv <- cbind(x1_x2,u,v)
uv
# 
uv_dens <- table(uv[,3],uv[,4])/36
uv_dens
