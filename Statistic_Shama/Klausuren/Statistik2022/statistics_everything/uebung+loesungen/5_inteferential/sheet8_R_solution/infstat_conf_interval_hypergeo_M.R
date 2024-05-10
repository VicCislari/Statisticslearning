########################################################
# Consider an urn with M white balls and N-M black. n 
# balls are drawn without replacement and X denotes the
# number of white balls in the sample. N=500 and n=50 
# are known but M the number of white balls is unknown.
# Construct an two sided 1-alpha=0.95 confidence intervall
# for M based on the H(N,M,n)-distribution of X. Compare 
# it with a binomial and a normal approximation.
#
# file: infstat_conf_interval_hypergeo_M.R
########################################################

library(tidyverse)

# urn modell: N total number of balls, M = number of white
# balls, n = number of drawn balls
# X = number of white balls ~ H(N,M,n)
N <- 500
n <- 50
alpha <- 0.05

# symmetric intervals [lb,ub] for X with probability 1-alpha 
# for different values of M
sy.intervals <- tibble(
  M = 0:N,
  # quantils of H(N,M,n)
  lb = qhyper(alpha/2,M,N-M,n),  
  ub = qhyper(1-alpha/2,M,N-M,n)
)
# plot of the the intervals
plot(x=sy.intervals$M, y=sy.intervals$lb, col="blue", 
     type = "p",
     xlab = "M", ylab = "lower and upper bounds",
     main = "symmetric 95% intervals for X")
points(x=sy.intervals$M, y=sy.intervals$ub, col="red")

# Mention the lb- and ub-functions are not strictly monotonously
# increasing: use for given value of X the min of the 
# corresponding ub values and the max of the corresponding lb 
# values of M as an inverse of the two function. These values
# are the bounds of the confidence intervals.
ex.conf.intervall <- function(x) {
  return(c(
    sy.intervals %>%
      filter(ub == x) %>% 
      mutate(l = min(M)) %>% 
      select(l) %>% 
      unique() %>% 
      as.numeric(),
    sy.intervals %>%
      filter(lb == x) %>%
      mutate(u = max(M)) %>% 
      select(u) %>% 
      unique() %>% 
      as.numeric()
  ))
}

# The binom.test(x,n) function returns in the variable 
# conf.int the confidence interval for p=M/N if they are X
# white balls in a sample of n balls drawn from the urn
# with replacement
binom.appr.conf.intervall <- function(x) {
  return(
    c(
      binom.test(x, n, conf.level = 1-alpha)$conf.int[1]*N,
      binom.test(x, n, conf.level = 1-alpha)$conf.int[2]*N
    )
  )
}

# normal approximation of the confidence interval for an
# unknown proportion if x white balls are in a sample of
# n balls drwan with replacement
normal.appr.conf.intervall <- function(x) {
  return(
    c(
      N*(x/n -qnorm(1-alpha/2)*sqrt(x*(1-x/n)/n^2)),
      N*(x/n +qnorm(1-alpha/2)*sqrt(x*(1-x/n)/n^2))
    )
  )
}

# tibble of the bounds of the confidence intervalls for M
# for all possibloe values of X
tab <- tibble(
  X = 0:n) %>% 
  group_by(X) %>%
  mutate(ex.lb=ex.conf.intervall(X)[1],
         ex.ub=ex.conf.intervall(X)[2],
         binom.lb=binom.appr.conf.intervall(X)[1],
         binom.ub=binom.appr.conf.intervall(X)[2],
         norm.lb=normal.appr.conf.intervall(X)[1],
         norm.ub=normal.appr.conf.intervall(X)[2]
         )

# plot of all bounds
plot(x=tab$X, y=tab$ex.lb, col="red",
     xlab = "x", ylab = "M",
     main = "95% confidence intervall for M in H(N=500,M,n=50)",
     sub = "red = exact, blue = binomial approx, black = normal approx.")
points(x=tab$X, y=tab$ex.ub, col="red")
points(x=tab$X, y=tab$binom.lb, col="blue")
points(x=tab$X, y=tab$binom.ub, col="blue")
points(x=tab$X, y=tab$norm.lb, col="black")
points(x=tab$X, y=tab$norm.ub, col="black")