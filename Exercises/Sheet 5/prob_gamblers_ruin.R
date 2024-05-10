###############################################################################
# Consider two gamblers whose capitals sum to 7 dollar, so that as soon as one 
# has all seven dollars the other is ruined and the game stops.  Plays form 
# independent trials with even chances for winning and losing. Let X[n] be the 
# capital of the first gambler at the end of the nth play.
# a) Determine the probabilities of the values of X[3] if X[1]=3.
# b) Simulate for every possible initial value of X[1] the values of X[10].
# c) Describe the probabilities P(X[2]=j | X[1]=j) by 8x8 matrix P.
# d) Show that P^n contains the probabilities P(X[n]=j | X[1]=i).
# e) Evaluate P^10 and compare it with the relative frequencies of X[10] given 
#    by 100 simulations.
# f) Estimate the probabilites of the stop of the by evaluating P^50
#
# file: prob_gamblers_ruin.R
###############################################################################
library(tidyverse)

# a) 
# X[1]                        3 
#                 0.5|                  | 0.5
# X[2]               2                  4
#             0.5|       |0.5    0.5|      |0.5      
# X[3]           1       3          3      5
#               0.25    0.25       0.25   0.25

# c) transition matrix
P <- matrix(data = 0, ncol = 8, nrow = 8, byrow = FALSE)
P[1,1] <- 1
P[8,8] <- 1
for (i in 2:7) {
  P[i,i-1] <- 0.5
  P[i,i+1] <- 0.5
}
P

# c)  Simulation
n.sim <- 10
sim.games <- matrix(data = 0, nrow = 6, ncol = n.sim, byrow = TRUE)
sim.games[,1] <- 1:6
# store whole games
for (j in 1:6) {
  for (i in 2:n.sim) {
    sim.games[j,i] <- case_when(
      sim.games[j,i-1]==0 ~ 0,
      (sim.games[j,i-1] < 7) & (sim.games[j,i-1] > 0) ~ 
        sample(c(sim.games[j,i-1]-1,sim.games[j,i-1]+1), size = 1, replace = FALSE),
      sim.games[j,i-1]==7 ~ 7
    )
  }
}
colnames(sim.games) <- rep("value.",n.sim) %>% paste(1:n.sim, sep="")
rownames(sim.games) <- rep("init.",6) %>% paste(1:6, sep="")
sim.games

# d) P( X[10] = j | X[1]=i ) = P^n_(i,j) Chapman-Kolmogoroff equation

# e) 
P.10 <- P
for (i in 2:10) {
  P.10 <- P.10 %*% P
}
colnames(P.10) <- rep("X[10]=",8) %>% paste(0:7, sep="")
rownames(P.10) <- rep("X[1]=",8) %>% paste(0:7, sep="")
P.10 %>% round(digits=3)
# X[10]=0 X[10]=1 X[10]=2 X[10]=3 X[10]=4 X[10]=5 X[10]=6 X[10]=7
# X[1]=0   1.000   0.000   0.000   0.000   0.000   0.000   0.000   0.000
# X[1]=1   0.754   0.041   0.000   0.087   0.000   0.064   0.000   0.054
# X[1]=2   0.549   0.000   0.128   0.000   0.151   0.000   0.064   0.107
# X[1]=3   0.344   0.087   0.000   0.192   0.000   0.151   0.000   0.226
# X[1]=4   0.226   0.000   0.151   0.000   0.192   0.000   0.087   0.344
# X[1]=5   0.107   0.064   0.000   0.151   0.000   0.128   0.000   0.549
# X[1]=6   0.054   0.000   0.064   0.000   0.087   0.000   0.041   0.754
# X[1]=7   0.000   0.000   0.000   0.000   0.000   0.000   0.000   1.000

# store X[10] in 100 simulations
final.values <- matrix(data = 0, ncol = 6, nrow = 100, byrow = TRUE)
for (k in 1:100) {
  for (j in 1:6) {
    g <- rep(0,n.sim)
    g[1] <- j
    for (i in 2:n.sim) {
      g[i] <- case_when(
        g[i-1]==0 ~ 0,
        (g[i-1] < 7) & (g[i-1] > 0) ~ 
          sample(c(g[i-1]-1,g[i-1]+1), size = 1, replace = FALSE),
        g[i-1]==7 ~ 7
      )
    }
    final.values[k,j] <- g[n.sim]
  }
}
final.values

# estimation the ratios of the final values after n.sim games
tibble(value = 0:7) %>%
  left_join(final.values %>% as_tibble() %>% count(V1) %>% 
              rename(value = V1, init.1 = n), by = "value") %>%
  left_join(final.values %>% as_tibble() %>% count(V2) %>% 
              rename(value = V2, init.2 = n), by = "value") %>%
  left_join(final.values %>% as_tibble() %>% count(V3) %>% 
              rename(value = V3, init.3 = n), by = "value") %>%
  left_join(final.values %>% as_tibble() %>% count(V4) %>% 
              rename(value = V4, init.4 = n), by = "value") %>%
  left_join(final.values %>% as_tibble() %>% count(V5) %>% 
              rename(value = V5, init.5 = n), by = "value") %>%
  left_join(final.values %>% as_tibble() %>% count(V6) %>% 
              rename(value = V6, init.6 = n), by = "value") %>%
  # exchange NA by 0 and divide by 50
  mutate(
    init.1 = replace_na(init.1, 0)/100,
    init.2 = replace_na(init.2, 0)/100,
    init.3 = replace_na(init.3, 0)/100,
    init.4 = replace_na(init.4, 0)/100,
    init.5 = replace_na(init.5, 0)/100,
    init.6 = replace_na(init.6, 0)/100) -> estimated.ratios
estimated.ratios %>% as.matrix() %>% t() -> M
M <- M[2:7,]
colnames(M) <-  rep("X[10]=",8) %>% paste(0:7, sep="")
rownames(M) <- rep("X[1]=",6) %>% paste(1:6, sep="")
M
# A tibble: 8 x 7
# value init.1 init.2 init.3 init.4 init.5 init.6
# <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
# 1     0   0.76   0.46   0.3    0.18   0.11   0.03
# 2     1   0      0.07   0      0.12   0      0.04
# 3     2   0.1    0      0.17   0      0.08   0   
# 4     3   0      0.19   0      0.2    0      0.08
# 5     4   0.08   0      0.22   0      0.2    0   
# 6     5   0      0.14   0      0.21   0      0.07
# 7     6   0.04   0      0.1    0      0.08   0   
# 8     7   0.02   0.14   0.21   0.29   0.53   0.78

# f)
n <- 50
P.50 <- P
for (i in 2:50) {
  P.50 <- P.50 %*% P
}
colnames(P.50) <- rep("X[50]=",8) %>% paste(0:7, sep="")
rownames(P.50) <- rep("X[1]=",8) %>% paste(0:7, sep="")
# aproximation of a stop with value=0 or value=7 
P.50[2:7,c(1,8)]
#         X[50]=0   X[50]=7
# X[1]=1 0.8555890 0.1414571
# X[1]=2 0.7117630 0.2829143
# X[1]=3 0.5679370 0.4254256
# X[1]=4 0.4254256 0.5679370
# X[1]=5 0.2829143 0.7117630
# X[1]=6 0.1414571 0.8555890

# exact values
# transition probabilities from the transient states 1 to 6 to the
# absorbing states 0 and 7

# probabilities of a transition from a transient state to another transient state
Q <- P[2:7,2:7] 
# probabilities of a transition from a transient state in an absorbing state
B <- P[2:7,c(1,8)]
# B.n = (I+Q+Q^2+...+Q°(n-1))*B, B.n[i,j] is the probanility that starting from
# a transient state i (1,...,6) to enter an absorbing state j (0,7) at or before
# the n-th step
# G=(I+Q+Q^2+...)*B = (I-Q)^-1 * B, G[i,j] is the probability of ever reaching
# an absorbing state j from a transient state i

I <- matrix(data=0, nrow = 6, ncol = 6, byrow = TRUE)
for (i in 1:6) I[i,i] <- 1
# solve(I-Q) is the inverse of (I-Q)
solve(I-Q) %*% B -> G
colnames(G) <- c("value=0","value=7")
rownames(G) <- rep("start with",6) %>% paste(1:6)
G
#               value=0   value=7
# start with 1 0.8571429 0.1428571
# start with 2 0.7142857 0.2857143
# start with 3 0.5714286 0.4285714
# start with 4 0.4285714 0.5714286
# start with 5 0.2857143 0.7142857
# start with 6 0.1428571 0.8571429

