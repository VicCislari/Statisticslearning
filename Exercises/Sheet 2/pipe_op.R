###############################################
# Sheet II: pipe operator
#
# file: pipe_op.R
###############################################

# load libraries
library(tidyverse)

# 2 %>%-Operator
# a)
sin(log((5+3)**0.5))
# or 
(5+3) %>% sqrt() %>% log() %>% sin()
# or
5 %>% sum(3) %>% sqrt() %>% log() %>% sin()
# b)
v <- seq(from = 0.5, to = 5, by = 0.5)
v

round(sum(log(v**2)),2)
# %>%-operator
v %>% 
  '^'(2) %>% 
  log() %>% 
  sum() %>% 
  round(2) 