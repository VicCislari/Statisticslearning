#########################################################
# To determine the number of N of red deers living in a
# precinct region 7 red deer were caught and marked in a 
# trapping action. Afterwards the animals were again 
# released. After a certain time, another trapping action 
# was started. Thereby 3 red deer were caught, whereby 2 
# already were marked. It is assumed that between the no 
# influx or outflow of red deer in the region and that the 
# animals were not able to pass the region within a short 
# period of time. 
# Determine a maximum likelihood estimator for the total 
# number N of the red deer living in the region.
#
# file: max_likelihood_deers_sol.R
#########################################################

library(tidyverse)
library(xtable)

n.marked <- 7
n.caught.1 <- 3
n.rd.1 <- 2
n.caught.2 <- 8
n.rd.2 <- 4

# create a tibble with prob. of the observation dep. on N
ml.est.N <- tibble(
  N = n.marked:50,
  est.1 = dhyper(x=n.rd.1,m=n.marked,n=N-n.marked,k=n.caught.1),
  est.2 = est.1 * 
    dhyper(x=n.rd.2,m=n.marked+(n.caught.1-n.rd.1),
           n=N-n.marked-(n.caught.1-n.rd.1),k=n.caught.2)
)
head(ml.est.N,20)

# diagramm of the likelihood functions
plot(x = ml.est.N$N, y = ml.est.N$est.1, col = "blue",
     xlab = "N", ylab = "prob. of the observation",
     main = "Likelihod function",
     sub = "blue = first caught, red: first and second caught")
points(x = ml.est.N$N, y = ml.est.N$est.2, col = "red")

# find the maxima
ML.EST.1 <- ml.est.N %>%
  select(N, est.1) %>%
  filter(est.1 == max(est.1))
ML.EST.2 <- ml.est.N %>%
  select(N, est.2) %>%
  filter(est.2 == max(est.2))
ML.EST.1; ML.EST.2