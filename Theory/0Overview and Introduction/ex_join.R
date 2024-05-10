#########################################
# Joining data
# Examples
#
# file: ex_join.R
#########################################

# tidyverse einbinden
library(tidyverse)

# tables
x <- tibble(key = c(1,2,3), val_x = c("x1","x2","x3"))
x
y <- tibble(key = c(1,2,4), val_y = c("y1","y2","y3"))
y

# inner join
x %>% 
  inner_join(y)

# left join
x %>% 
  left_join(y,by = "key")

# right join
x %>% right_join(y,by = "key")

# full join
x %>% full_join(y,by = "key")

# no unique keys
x <- tibble(key = c(1,2,2,4), val_x = c("x1", "x2", "x3", "x4"))
x
y <- tibble(key = c(1,2,2,3), val_y = c("y1", "y2", "y3", "y4"))
y
x %>% 
  left_join(y,by = "key")

