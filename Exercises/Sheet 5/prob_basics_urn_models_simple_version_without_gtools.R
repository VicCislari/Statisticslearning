################################################################
# Urn Models
# Elementary events and their probabilities: direct 
# determination 
#
# file: prob_basics_urn_models_simple_version_without_gtools.R
################################################################
# A large number of discrete probability spaces can be traced back to
# so-called urn models. An urn contains n balls, which do not all have
# to be different. From these urns r balls are drawn with or without
# replacement. For the result of the drawing, the order or only
# the quantity of the drawn balls can be of importance. 
# Here an urn with 10 balls is considered. 5 of them are red, 3 balls
# are blue and 2 balls are green. 3 balls are drawn. The following 4
# cases should be distinguished:
# I Drawing with replacement with respect to the order
# II Drawing with replacement without observing the order
# III Drawing without replacement with respect to the order
# IV Drawing without replacement without observing the order
# a) Determine suitable event spaces and its sizes to describe the random 
# experiment. 
# b) Determine the probabilities of all elementary events in $\Omega$
# using a Laplace model, i.e. as a determination of the ratio of the
# number of favorable cases by the number of all cases. 
# Hint: To determine the probabilities with R, assume that the
# n balls are numbered consecutively, i.e. they are distinguishable,
# and that the order is first observed in a drawing. Every
# r-variation of the numbers 1 to n is equally probable. Determine
# the set of all these drawings and map each drawing to the corresponding 
# elementary event. By dividing the number of drawings belonging to an 
# elementary event and the number of all drawings, you can obtain the 
# corresponding probabilities. 

library(tidyverse)

# Bag with 10 balls: 5*red, 3*blue und 2*green 
bag <- rep(c("r","b","g"),c(5,3,2))
bag

# I: sampling r=3 balls with replacement regarding the order
sample.with.repl.ordered <- tibble(
  # construct all possible r-variations of 1,2,...,10
  x.1 = rep(1:100, each=100, length.out=10^3),
  x.2 = rep(1:10, each=10, length.out=10^3),
  x.3 = rep(1:10,length.out = 10^3)
) %>%
  # the following operations must be performed rowwise
  rowwise() %>%
  mutate(
    # maps the numbered balls to the corresponding colors
    event = c(bag[x.1],bag[x.2],bag[x.3]) %>% paste(collapse = "")
  ) %>%
  # regard only the events
  select(event) %>%
  # count the number of elementary events; all are equally like
  group_by(event) %>%
  mutate(
    prob = n()/10^3
  ) %>%
  # remove duplicates
  unique()

# II: sampling 3 balls with replacement regarding the order
sample.with.repl.unordered <- tibble(
  # construct all possible r-variations
  x.1 = rep(1:100, each=100, length.out=10^3),
  x.2 = rep(1:10, each=10, length.out=10^3),
  x.3 = rep(1:10,length.out = 10^3)
) %>%
  # the following operations must be performed rowwise
  rowwise() %>%
  mutate(
    # maps the numbered balls to the corresponding colors and consider only the
    # set of drawan colors
    event = c(bag[x.1],bag[x.2],bag[x.3]) %>% sort() %>% paste(collapse = "")
  ) %>%
  # regard only the events
  select(event) %>%
  # count the number of elementary events; all are equally like
  group_by(event) %>%
  mutate(
    prob = n()/10^3
  ) %>%
  # remove duplicates
  unique()

# III: sampling r=3 balls without replacement und regard the order
sample.without.repl <- matrix(data=0, ncol = 3, nrow = 10*9*8)
# construct all possible r-variations
l <- 1
for (i in 1:10) {
  for (j in setdiff(1:10,i)) {
    for (k in setdiff(1:10,c(i,j))) {
      sample.without.repl[l,] <- c(i,j,k)
      l <- l+1
    }
  }
} 

sample.without.repl %>%
  # convert to a tibble
  as_tibble() %>%
  # the following operations must be performed rowwise
  rowwise() %>%
  mutate(
    # maps the numbered balls to the corresponding colors
    event = c(bag[V1],bag[V2],bag[V3]) %>% paste(collapse = "")
    ) %>%
  # consider only the elementary events
  select(event) %>%
  # counts the number of elementary events (all are equally like)
  group_by(event) %>%
  mutate(
    prob = n()/(10*9*8)
  ) %>%
  unique() -> sample.without.repl.ordered

# IV: sampling 3 balls without replacement without regarding the order
sample.without.repl %>%
  # convert to a tibble
  as_tibble() %>%
  # the following operations must be performed rowwise
  rowwise() %>%
  mutate(
    # maps the numbered balls to the corresponding colors and consider only the
    # set of drawan colors
    event = c(bag[V1],bag[V2],bag[V3]) %>% sort() %>% paste(collapse = "")
  ) %>%
  # consider only the elementary events
  select(event) %>%
  # counts the number of elementary events (all are equally like)
  group_by(event) %>%
  mutate(
    prob = n()/(10*9*8)
  ) %>%
  unique() -> sample.without.repl.unorderd

