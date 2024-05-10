########################################################
# Urn Models
# Elementary events and their probabilities: Evaluation
# and determination applying the R commands combinations() 
# and permutations() in the gtools package.
#
# file: prob_basics_urn_models.R
########################################################

library(gtools)
library(tidyverse)

# combinations enumerates the possible combinations of a specified 
# size from the elements of a vector. permutations enumerates the 
# possible permutations.
# Usage
# combinations(n, r, v=1:n, set=TRUE, repeats.allowed=FALSE)
# permutations(n, r, v=1:n, set=TRUE, repeats.allowed=FALSE)
# Arguments
# n size of the source vector
# r	size of the target vectors
# v source vector
# set	= logical flag indicating whether duplicates should be removed 
#       from the source vector v. 
# repeats.allowed = logical flag indicating whether the constructed 
#                   vectors may include duplicated values. 
#
# Value: Returns a matrix where each row contains a vector of length r.

# Examples Consider the bags 
b1 <-  c("a","b","c") 
b2 <- c("a","a","b","c")
# and list all combinations and all permutations of order 2 if duplicated 
# elements in the outpot are allowd or not allowed.
combinations(n=3,r=2,v=b1)
combinations(n=3,r=2,v=b1,repeats.allowed = TRUE)

permutations(n=3,r=2,v=b1)
permutations(n=3,r=2,v=b1,repeats.allowed = TRUE)

# n = number of distinct elements of source vector, if set = TRUE!!
combinations(n=3,r=2,v=b2, set = TRUE) 
combinations(n=4,r=2,v=b2, set = FALSE)
combinations(n=4,r=2,v=b2, set = FALSE, repeats.allowed = TRUE)

######################################################################
# Bag with 10 balls: 5*red, 3*blue und 2*green 
# 3 balls are drawn
bag <- rep(c("r","b","g"),c(5,3,2))
bag

######################################################################
# Use the function sample() to determine the result of 10 random draws. 

sample(x=bag, size=10) # without replacement
sample(x=bag, size=10, replace=TRUE) # with replacement

######################################################################
# Drawing with replacement with respect to the order
######################################################################
# Elementary events and their probabilities directly calculated
permutations(n=3, r=3, v=bag, set = TRUE, repeats.allowed = TRUE) %>%
  as_tibble(.name_repair = "universal") %>%
  # Treatment of problematic column names 
  # .name_repair	= "universal": Make the names unique and syntactic
  mutate(
    V1 = ...1, V2 = ...2, V3 = ...3,
    prob = case_when(
      V1 == "r" ~ 0.5,
      V1 == "b" ~ 0.3,
      V1 == "g" ~ 0.2) * 
      case_when(
        V2 == "r" ~ 0.5,
        V2 == "b" ~ 0.3,
        V2 == "g" ~ 0.2) * 
      case_when(
        V3 == "r" ~ 0.5,
        V3 == "b" ~ 0.3,
        V3 == "g" ~ 0.2
      ) 
  ) %>% select(V1,V2,V3,prob) %>%
  # sort
  arrange(V1,V2,V3) -> M1
M1

# Elementary events and their probabilities determined by counting all 
# equally like 3 permutations of numbers 1 to 10 (balls are numbererd
# from 1 to 10)
permutations(n=10, r=3, v=1:10, set = TRUE, repeats.allowed = TRUE) %>% 
  as_tibble() %>% 
  # zeilenweises Vorgehen
  rowwise() %>%
  mutate(
    # Übersetzung der 3-Permutation von 1 bis 10 in die Farben der
    # gezogenen Kugeln
    event = paste(c(bag[V1],bag[V2],bag[V3]), collapse = " ")) %>% 
  # Zählen der Häufigkeiten der gezogenen Farben
  group_by(event) %>%
  summarise(count = n()) %>%
  # W. = Häufigkeit/Gesamtanzahl
  mutate(prob = count/sum(count)) %>%
  # sortieren
  arrange(event) -> M2

cbind(M1,M2)

# generate a latex table
library(xtable)
M2 %>% select(event, prob) %>% xtable(digits = 3)
M1

######################################################################

######################################################################
# Drawing with replacement without respect to the order
######################################################################
# Elementary events and their probabilities directly calculated
# Multinomial distribution of the number of drawn coloured balls
library(stats)
combinations(n=3, r=3, v=bag, set = TRUE, repeats.allowed = TRUE) %>%
  as_tibble() %>%
  # zeilenweises Vorgehen
  rowwise() %>%
  mutate(
    anz_r = if_else(V1 == "r",1,0) + 
      if_else(V2 == "r",1,0) + if_else(V3 == "r",1,0) ,
    anz_b = if_else(V1 == "b",1,0) + 
      if_else(V2 == "b",1,0) + if_else(V3 == "b",1,0) ,
    anz_g = if_else(V1 == "g",1,0) +
      if_else(V2 == "g",1,0) + if_else(V3 == "g",1,0) ,
    prob = dmultinom(x = c(anz_r, anz_b, anz_g), size = 3,
                     prob = c(0.5, 0.3, 0.2))
  ) %>%
  select(V1, V2, V3, prob) %>%
  # sortieren
  arrange(V1, V2, V3) -> M2

# Elementary events and their probabilities determined by counting all 
# equally like 3 combinations of numbers 1 to 10 (balls are numbererd
# from 1 to 10)
permutations(n=10, r=3, v=1:10, set = FALSE, repeats.allowed = TRUE) %>% 
  as_tibble() %>% 
  # zeilenweises Vorgehen
  rowwise() %>%
  mutate(
    # pro Zeile sortieren nach den Farben und Umwandlung des sortierten 
    # Vektors in einen String
    event = paste(sort(c(bag[V1],bag[V2],bag[V3])), 
      collapse = " ")) %>% 
  # Zählen der Häufigkeiten der gezogenen Farbkombinationen
  group_by(event) %>%
  summarise(count = n()) %>%
  # W. = Häufigkeit/Gesamtanzahl
  mutate(prob = count/sum(count)) %>%
  # sortieren
  arrange(event) -> M22

# generate a latex table
M22 %>% select(-count) %>% xtable(digits = 3)

#######################################################################

######################################################################
# Drawing without replacement with respect to the order
######################################################################
# Elementary events and their probabilities determined by counting all 
# equally like 3 permutations (without repetitions) of numbers 1 to 10 
# (balls are numbererd from 1 to 10)
permutations(n=10, r=3, v=bag, set = FALSE, repeats.allowed = FALSE) %>% 
  as_tibble() %>% 
  # zeilenweises Vorgehen
  rowwise() %>%
  mutate(
    # Übersetzung der 3-Permutation von 1 bis 10 in die Farben der
    # gezogenen Kugeln
    event = paste(c(V1,V2,V3), collapse = " ")) %>% 
  group_by(event) %>%
  # Zählen der Häufigkeiten der gezogenen Farben
  summarise(count = n()) %>%
  # W. = Häufigkeit/Gesamtanzahl
  mutate(prob = count/sum(count)) %>% arrange(event) -> M3

# generate a latex table
M3 %>% select(-count) %>% xtable(digits = 3)

# some theoretical results
# P((b,r,b)) = P(b) * P(r|b) * P(b|(b,r)) = 3/10 * 5/9 * 2/8
# P((g,b,b)) = P(g) * P(b|g) * P(b|(g,b)) = 2/10 * 3/9 * 2/8
# ...

# Let (x,y,z) be an elementary event. I's probability depends only on the
# number of drawn coloured balls:
# P((x,y,z)) = P(R=i,B=j,G=k) = 
#   fallende.Faktorielle(5,i) * fallende.Faktorielle(3,j) * fallende.Faktorielle(2,k) / (10*9*8)
permutations(n=10, r=3, v=bag, set = FALSE, repeats.allowed = FALSE) %>%
  as_tibble() %>% 
  rowwise() %>%
  mutate(
    anz_r = if_else(V1 == "r",1,0) + 
      if_else(V2 == "r",1,0) + if_else(V3 == "r",1,0) ,
    anz_b = if_else(V1 == "b",1,0) + 
      if_else(V2 == "b",1,0) + if_else(V3 == "b",1,0) ,
    anz_g = if_else(V1 == "g",1,0) +
      if_else(V2 == "g",1,0) + if_else(V3 == "g",1,0)) %>%
  unique() %>%
  mutate(
    prob = (factorial(5)/factorial(5-anz_r)) * 
      (factorial(3)/factorial(3-anz_b)) *
      (factorial(2)/factorial(2-anz_g)) / (10*9*8)) %>%
  arrange(V1,V2,V3) 
#######################################################################

######################################################################
# Drawing without replacement without respect to the order
######################################################################
# Ereignisraum mit theoretischen W.
combinations(n=10, r=3, v=bag, set = FALSE, repeats.allowed = FALSE) %>% 
  as_tibble() %>%
#  mutate(
#    V1 = bag[V1],
#    V2 = bag[V2],
#    V3 = bag[V3]) %>% 
  unique() %>%
  # Bestimmung der Wahrscheinlichkeiten: zeilenweise werden zunächst die
  # Anzahl der gezogenen Farben bestimmt und anschließend ergeben sich die 
  # Wahrscheinlichkeiten durch Anwendung einer verallgemeinert hypergeo-
  # metrischen Verteilung
  rowwise() %>%
  mutate(
    event = paste(sort(c(V1,V2,V3)), collapse = " "),
    anz_r = if_else(V1 == "r",1,0) + 
      if_else(V2 == "r",1,0) + if_else(V3 == "r",1,0) ,
    anz_b = if_else(V1 == "b",1,0) + 
      if_else(V2 == "b",1,0) + if_else(V3 == "b",1,0) ,
    anz_g = if_else(V1 == "g",1,0) +
      if_else(V2 == "g",1,0) + if_else(V3 == "g",1,0) ,
    prob = choose(5,anz_r)*choose(3,anz_b)*choose(2,anz_g)/choose(10,3)) %>%
  # sortieren
  arrange(event) 

# Bestimmung der Elementarereignisse und ihrer W. durch Zählen aller
# gleichwahrscheinlichen 3-Kombinationen (ohne Wiederholungen) der Zahlen 
# 1 bis 10 (Kugeln sind von 1 bis nummeriert)
permutations(n=10, r=3, v=bag, set = FALSE, repeats.allowed = FALSE) %>%
  as_tibble() %>% 
  # zeilenweise sortieren und zu einem String zusammenfassen
  rowwise() %>%
  mutate(
    event = paste(sort(c(V1,V2,V3)), collapse = " ")) %>%
  # Zählen der Häufigkeiten der unterscheidlichen 3-Kombinationen
  group_by(event) %>%
  summarise(count = n()) %>%
  mutate(prob = count/sum(count)) %>%
  arrange(event) -> M4

# generate a latex table
M4 %>% select(-count) %>% xtable(digits=3)