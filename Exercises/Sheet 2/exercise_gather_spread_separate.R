############################################################
# Sheet 2: applications of gather(), spread() and separate()
#
# file exercise_gather_spread_separate.R
############################################################

library(tidyverse)
library(stringr)

# a) Consider the dataset who of the package tidyr.
# country, iso2, iso3, and year are already variables, so they can be left as is. 
# But the columns from new_sp_m014 to newrel_f65 encode four variables in their names:
# * The new_/new prefix indicates these are counts of new cases. This dataset only 
#    contains new cases, so we'll ignore it here because it's constant.
# * sp/rel/ep describe how the case was diagnosed.
# * m/f gives the gender.
# * 014/1524/2535/3544/4554/65 supplies the age range.
# Break these variables up by specifying multiple column names and make the dataset tidy.
who %>%
  # gather the values of the columuns new_sp_m014:newrel_f65 to the columns key and values
  gather(key = "key", value = "cases", new_sp_m014:newrel_f65, na.rm = TRUE) %>%
  # exchange the newrel by new_rel -> syntax of new: new_type_sexage
  mutate(key = str_replace(key,"newrel","new_rel")) %>%
  # separate the column key into 3 columns
  separate(key,c("new", "type", "sexage"), sep = "_") %>%
  # separate the column sexage into 2 columns
  separate(sexage, c("sex", "age"), sep = 1) %>%
  # remowe the column new
  select(-new, -iso2, -iso3) -> who.tidy

# b) The anscombe dataset contains four pairs of variables (x1 and y1, x2 and y2, etc.)
# that underlie Anscombe's quartet, a collection of four datasets that have the 
# same summary statistics (mean, sd, correlation etc), but have quite different data. 
# Produce a dataset with columns set, x and y.
anscombe[1:4] %>% 
  # gather the the first 4 columns to the columns set and x
  gather(key = "set", value = "x") %>%
  # remove the char x from all values of set
  mutate(set = str_remove(set, pattern ="x")) -> anscombe.x
# Do the same for the last 4 columns
anscombe[5:8] %>% 
  gather(key = "set", value = "y") %>%
  mutate(set = str_remove(set, pattern ="y")) -> anscombe.y
# create a tibble containing set, x and y
tibble(
  set = anscombe.x$set,
  x = anscombe.x$x,
  y = anscombe.y$y
  )

# c) Apply the following R statements
production <- 
  expand_grid(
    product = c("A", "B"), 
    country = c("AI", "EI"), 
    year = 2000:2014
  ) %>%
  filter((product == "A" & country == "AI") | product == "B") %>% 
  mutate(production = rnorm(nrow(.)))
production
# The data set production contains the combination of product, country, and year. 
# Widen the data so we have one column for each combination of product and country. 
production %>%
  mutate(pc = str_c(product, country, sep= "_")) %>%
  select(-product, -country) %>%
  spread(key = pc, value = production)

# d) The data set warpbreaks gives the number of warp breaks per loom, where a 
# loom corresponds to a fixed length of yarn for every combination of wool (A and B) 
# and tension (L, M, H).
# Produce a data set with the columns tension, A, B with the means of the breaks
warpbreaks %>%
  group_by(wool, tension) %>%
  summarise(Mean = mean(breaks)) %>%
  spread(key = wool, value = Mean)
