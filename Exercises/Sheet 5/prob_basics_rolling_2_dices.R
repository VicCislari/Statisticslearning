################################################################
# Rolling 2 dice: sample space and events 
#
# file: prob_basics_rolling_2_dices.R
################################################################
library(tidyverse)
# create sample space
Omega <- expand.grid(x=1:6, y=1:6) %>% as_tibble()
Omega
# A = first die = 1, B = sum of the scores is 6
A <- Omega %>% filter(x==1)  
A
B <- Omega %>% filter(x+y==6)
B
# A and B
intersect(A,B)
# A or B
union(A,B)
# not A and not B
intersect(setdiff(Omega,A),setdiff(Omega,B))
setdiff(Omega,union(A,B))
