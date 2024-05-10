# tidyverse einbinden
library(tidyverse)

################################################################
# Pipes in R Tutorial For Beginners: 
# https://www.datacamp.com/community/tutorials/pipe-r-tutorial
# Examples using pipe operator
# file: pipe_ex.R
################################################################

# f(x) can be rewritten as x %>% f
# 
sin(pi)
pi %>% 
  sin

# f(x, y) can be rewritten as x %>% f(y)
# 
round(pi,6)
pi %>% 
  round(6)

# x %>% f %>% g %>% h can be rewritten as h(g(f(x)))
cos(sin(pi))
pi %>% 
  sin %>% 
  cos

################################################################
# more complex examples
################################################################
# Initialize `x`
x <- c(0.109, 0.359, 0.63, 0.996, 0.515, 0.142, 0.017, 0.829, 0.907)
# Compute the logarithm of `x`, return suitably lagged and iterated differences, 
# compute the exponential function and round the result
round(exp(diff(log(x))),1)
# Do the same but now with `%>%`
x %>% 
  log %>% 
  diff %>% 
  exp %>% 
  round(1)
# diff returns differences of two following elements in a vector
diff(1:10)


# Import `babynames` data from package babynames
library(babynames)
# Load the data
data(babynames)
babynames
# Count how many young boys with the name "Taylor" are born
sum(select(filter(babynames,sex=="M",name=="Taylor"),n))
# Do the same but now with `%>%`
babynames %>% 
  filter(sex=="M",name=="Taylor") %>%
  select(n) %>%
  sum
