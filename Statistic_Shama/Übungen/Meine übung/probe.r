library(tidyverse)

x <- c(1:10,5,6)
x %>% unique()

sample_values<-tibble(
  #sample (x=c("f","m"),size = 6, replace = TRUE),
  x=sample ( c ("blond","female","male"),size = 10, replace = TRUE) 
)


sample_values %>% filter(x == c("male","blond")) # auslisten (filtern)

H <- ecdf(sample_values$x)

