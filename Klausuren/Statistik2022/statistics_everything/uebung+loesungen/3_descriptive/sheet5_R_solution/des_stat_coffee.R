#####################################################
# Descriptive Statistics: Exercise 4.1, 
# Heumann, Schomaker, page 90
#
# File: des_stat_coffee.R
#
###################################################### 

# load packages
library(tidyverse)
library(xtable) # necessary to generate tex-tables

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

# tex table
tex_tab <- xtable(rate)
print(tex_tab, include.rownames = FALSE, floating = FALSE)

# Spearman's rank correlation coefficient
R <- cor(x = rate$R_x, y = rate$R_y)
R
# or directly using the cor() function
cor(x = rate$x, y = rate$y, method = "spearman")

# change the the order of rating
rate <-
  rate %>%
  mutate(Rdesc_x = rank(-x)) %>%
  mutate(Rdesc_y = rank(-y))
# display 
rate

# tex table
tex_tab <- xtable(rate)
print(tex_tab, include.rownames = FALSE, floating = FALSE)

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

# tex table
tex_tab <- xtable(rate2)
print(tex_tab, include.rownames = FALSE, floating = FALSE)

# odds ratio
OR <- ((2/5)/(3/5))/((1/5)/(4/5))
OR

