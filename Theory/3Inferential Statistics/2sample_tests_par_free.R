##########################################################################
# Examples: parameter free 2 samples tests
#           sign test, Wilcoxon Signed Rank Test, 
#           Mann-Whitney U-Test, Wilcoxon Rank Sum Test
#
# file: 2sample_tests_par_free.R
##########################################################################

library(tidyverse)

##########################################################################
# Sign Test
##########################################################################
# Example: Lehn, Wegmann (5. Auflage), S. 165, Beispiel 3.54
# 20 cars are tested with two types of tyres. The braking distances for the 
# two types are given in the table for every car.
sample <- tibble(
  type.a = c(44.6, 55.0, 52.5, 50.2, 45.2, 46.0, 52.0, 50.2, 50.7, 49.2, 47.3, 
             50.1, 51.6, 48.7, 54.2, 46.1, 49.9, 52.3, 48.7, 56.9),
  type.b = c(44.7, 54.8, 55.6, 55.2, 45.6, 47.7, 53.0, 49.9, 52.2, 50.6, 46.1, 
             52.3, 53.9, 47.1, 57.2, 52.7, 49.0, 54.9, 51.4, 56.1)
)

# add variables containing the diffrences and the signs
sample <- sample %>%
  mutate(
    diff.ab = type.a - type.b,
    vorz = if_else(diff.ab < 0, "-", "+")
  )

# tex Tabelle erzeugen
library(xtable)
tex_tab <- xtable(sample)
print(tex_tab, floating = FALSE,
      file = "c:/Users/Falkenberg/OneDrive/Statistik/Slides/R_tex_files/ex_sign_test.tex")

# number of negative and positive differences
n.vorz <- sample %>%group_by(vorz) %>% summarise(n())
n.pos <- n.vorz$`n()`[2]
n.neg <- n.vorz$`n()`[1]

# Test
alpha <- 0.05
qbinom(c(alpha/2,1-alpha/2),nrow(sample),0.5); # 6; 14

binom.test(n.pos,n.neg,p=0.05,alternative="two.sided",conf.level=1-alpha)
#Exact binomial test
# data:  n.pos and n.neg
# number of successes = 6, number of trials = 14, p-value = 3.309e-05
# alternative hypothesis: true probability of success is not equal to 0.05
# 95 percent confidence interval:
#    0.1766111 0.7113906
# sample estimates:
#   probability of success 
#   0.4285714 

##########################################################################
# Wilcoxon Signed Rank Test
##########################################################################
# add the the ranks
sample <- sample %>% 
  mutate(
    diff.abs.value = abs(diff.ab),
    rang = rank(diff.abs.value)
  )

xtable(sample)

# Determining of R- and R+
sample %>% filter(vorz == "-") %>%
  summarise(R = sum(rang)) %>% as.numeric() -> r.neg
r.pos <- 0.5*nrow(sample)*(1+nrow(sample)) - r.neg

# Test
alpha <- 0.05
# Distribution of the Wilcoxon Signed Rank Statistics
qsignrank(c(alpha/2,1-alpha/2),nrow(sample)) # 53; 157

# normal approximations of the quantiles
n <- nrow(sample)
exp.val.r.neg <- n*(n+1)/2
var.r.neg <- n*(n+1)*(2*n+1)/24
qnorm(c(alpha/2,1-alpha/2), mean = exp.val.r.neg, sd = var.r.neg)

wilcox.test(sample$type.a, sample$type.b, alternative="two.sided",
            paired=TRUE,conf.int=TRUE, conf.level=1-alpha)
#	Wilcoxon signed rank test
# data:  sample$type.a and sample$type.b
# V = 35, p-value = 0.007296
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
# -2.4 -0.4
# sample estimates:
#   (pseudo)median 
# -1.275 

##########################################################################
# Mann-Whitney U-Test, Wilcoxon Rank Sum Test
##########################################################################
# Example: In a genetic inheritance study samples of individuals from 
# several ethnic groups were taken. Blood samples were collected from each 
# individual and several variables measured. We compare the groups labeled 
# ``Native American'' and ``Caucasian'' with respect to the variable MSCE 
# (mean sister chromatid exchange). The data is as follows:\\[0.5cm]
AM <- c(8.50,9.48,8.65,8.16,8.83,7.76,8.63)
CA <- c(8.27,8.20,8.25,8.14,9.00,8.10,7.20,8.32,7.70)
n1 <- length(AM); n2 <- length(CA)

# Determining the ranks
sample <- tibble(
  grp = c(rep("AM",n1), rep("CA",n2)),
  MSCE = c(AM,CA),
  rang = rank(MSCE)
)

# Determining of R.AM and R.CA
sample %>% filter(grp == "AM") %>% summarise(sum(rang)) %>% 
  as.numeric() -> R.AM # 75
sample %>% filter(grp == "CA") %>% summarise(sum(rang)) %>% 
  as.numeric() -> R.CA # 61

# Determining of U.AM and U.CA
inversions <- as_tibble(expand.grid(AM,CA)) %>%
  mutate(
    AM = Var1,
    CA =Var2,
    z.ij = if_else(AM>CA, 1,0)
  ) %>% select(-Var1, -Var2)
U.CA <- sum(inversions$z.ij) # 47
U.AM <- nrow(inversions) - U.CA # 16

# Test
alpha <- 0.05
# Distribution of the Wilcoxon Rank Sum Statistic
qwilcox(c(alpha/2,1-alpha/2),n1,n2) # 13; 50

wilcox.test(AM, CA, conf.level=0.05)
# Wilcoxon rank sum exact test
# 
# data:  AM and CA
# W = 47, p-value = 0.1142
# alternative hypothesis: true location shift is not equal to 0

# Example: Heumann, Schoemaker, p. 233 ff
# Reaction time to a stimulus were measured in two groups
coffee <- c(3.7,4.9,5.2,6.3,7.4,4.4,5.3,1.7,2.9)
water <- c(4.5,5.1,6.2,7.3,8.7,4.2,3.3,8.9,2.6,4.8)
n.coffee <- length(coffee); n.water <- length(water)

# Bestimmung der Rangzahlen
study <- tibble(
  grp = c(rep("coffee",n.coffee), rep("water",n.water)),
  time = c(coffee,water),
  rang = rank(time)
)

# The teststatistic is computed from the ranks of the ordered sample values.
# Taking each observation in sample X and count the number of
# observations in sample Y that are smaller than it.
# The total of these counts is U_Y the teststatistic of the U-test. 

study %>% arrange(time) %>%
  group_by(time) %>%
  # count the numbers of observation of the other group < time
  mutate(u = if_else(grp=="coffee",sum(water < time),sum(coffee < time))) %>%
  ungroup() -> U
U.coffee <- U %>% filter(grp=="water") %>% select(u) %>% sum()
U.water <- U %>% filter(grp=="coffee") %>% select(u) %>% sum()

# Bestimmung von R.coffee und R.water
study %>% group_by(grp) %>% summarise(R = sum(rang)) %>% as_vector() -> R
filter(grp == "coffee") %>% summarise(sum(rang)) %>% 
  as.numeric() -> R.coffee # 83
study %>% filter(grp == "water") %>% summarise(sum(rang)) %>% 
  as.numeric() -> R.water # 107

# test statistic
U.coffee <- n.coffee*n.water +n.coffee*(1+n.coffee)*0.5 - R.coffee # 52
U.water <- n.coffee*n.water +n.water*(1+n.water)*0.5 - R.water # 38
U <- min(U.coffee,U.water) # 38
t.xy <- (U -n.coffee*n.water*0.5)/sqrt(n.coffee*n.water*(n.coffee+n.water+1)/12)
t.xy

alpha <- 0.05
# Distribution of the Wilcoxon Rank Sum Statistic
qwilcox(c(alpha/2,1-alpha/2),n.coffee,n.water) # 21; 69

# decision
abs(t.xy) < qnorm(1-0.05/2) # true -> no rejection
(qwilcox(alpha/2,n.coffee,n.water) < U.water ) & 
  (U.water < qwilcox(1-alpha/2,n.coffee,n.water)) # true -> no rejection

wilcox.test(coffee, water, alternative = "two.sided", paired = FALSE,
            conf.level = 1-0.05)
# Wilcoxon rank sum exact test
# 
# data:  coffee and water
# W = 38, p-value = 0.6038
# alternative hypothesis: true location shift is not equal to 0

# mention that W = U.water; if you conduct wilcox.test(water,coffee) W = U.coffee