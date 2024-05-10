library(tidyverse)

library(nycflights13)
flights %>% filter (arr_delay > 120)

flights %>% filter (arr_delay & dep_delay <= 0)

 