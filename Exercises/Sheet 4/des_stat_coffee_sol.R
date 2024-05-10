#####################################################
# Descriptive Statistics: Exercise 4.1, 
# Heumann, Schomaker, page 90
# Solution
#
# File: des_stat_coffee_sol.R
#
###################################################### 

# load packages
library(tidyverse)

# data
rate <-
  tibble(
    cafe = c(1,2,3,4,5),
    x = c(3,8,7,9,5),
    y = c(6,7,10,8,4)
  )
# add the ranks
rate <- 
  rate %>% 
  mutate(R_x = rank(x)) %>%
  mutate(R_y = rank(y))
# display data
rate

# Spearman's rank correlation coefficient
R <- cor(x = rate$R_x, y = rate$R_y)
R
# or directly using the cor() function
cor(x = rate$x, y = rate$y, method = "spearman")

# change the order of rating
rate <-
  rate %>%
  mutate(Rdesc_x = rank(-x)) %>%
  mutate(Rdesc_y = rank(-y))
# display 
rate

# Spearman's rank correlation coefficient
Rdesc <- cor(x = rate$Rdesc_x, y = rate$Rdesc_y)
Rdesc

# only the ratings good and bad
rate2 <- 
  tibble(
  quality = c("bad","good"),
  x = c(2,3),
  y = c(1,4)
  )
# display
rate2

# using R commands to determine rate2
tibble(
  cafe = c(1,2,3,4,5),
  x = c(3,8,7,9,5),
  y = c(6,7,10,8,4)
) %>% 
  gather(x,y, key = "journalist", value = "rating") %>%
  mutate(
    rating = if_else(rating > 5, "good", "bad")
  ) %>%
  select(-cafe) %>% 
  table() %>% 
  addmargins() -> rate2
rate2
  
# Conditional distributions
# 2:5 = proportion of bad ratings by journalist X
# 2:3 = proportion of journalist X ratings in the set of bad ratings
# relative risk: ratio of conditional distributions
# (2:5) : (1:5) = 2 = proportion of bad ratings is 2 times higher among ratings
#                     by journalist X when compared with ratings by journalist Y
# (2:3) : (3:7) = 1.55 = proportion of ratings by journalist X is 1.55 times 
#                        higher among bad ratings when compared with good ratings
# odds ratio
OR <- ((2/5)/(3/5))/((1/5)/(4/5))
OR
# (2:1) : (3:4) = 2.66 = chance of bad ratings is 2.66 times higher for ratings
#                        by journalist X when compared with good ratings
# (2:3) : (1:4) = 2.66 = chance of ratings by journalist X is 2.66 times higher 
#                        for bad ratings when compared with bad ratings by journalist Y

