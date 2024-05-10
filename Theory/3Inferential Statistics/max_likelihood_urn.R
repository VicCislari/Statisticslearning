###############################################################
# Example: Maximum Likelihood Estimator
#
# An urn contains n=20 balls. r balls are red, b balls are black 
# and g balls are green. 5 balls are drawn with replacement:
#   red, black, black, green, black, green
# Objective: maximum likelihood estimation of r, b, g
################################################################
library(tidyverse)

# possible values: 1 <= r <= 15, 3 <= b <= 17, 2 <= g <= 16, r+b+g = 20
# g = 20-r-b

# probability of the sample: r*b*(b-1)*(b-2)*(20-r-b)(20-r-b-1)/(20*19*18*17*16*15)

# Create a data frame from all combinations of the possible values of r, b
expand.grid(1:15,3:17) %>%
  # convert it to a tibble
  as_tibble() %>%
  # add the variables r, b and p
  mutate(r = Var1,
         b = Var2,
         g = 20-r-b,
         p = r*b*(b-1)*(b-2)*(20-r-b)*(20-r-b-1)/(20*19*18*17*16*15)) %>%
  select(-Var1, -Var2) %>%
  # remove the variables Var1, Var2
  filter(r+b <= 20) %>%
  # select the observations with r+b <= 20
  filter(p == max(p))
  # find the obs. with max. p
