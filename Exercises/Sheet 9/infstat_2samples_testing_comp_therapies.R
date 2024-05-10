##########################################################################
# Exercise: Wilcoxon Mann Whitney U-Test
# 
# file: infstat_2samples_testing_comp_therapies.R
##########################################################################
library(tidyverse)
# Two therapies for a specific febrile illness are to be
# be compared. For this purpose, 4 and 6 randomly selected
# randomly selected patients and the duration of treatment in hours
# required for the patient to be  necessary for the patient to be
# free of fever. 
T1 <- c(89.75,94.5,98.75,101.5)
T2 <- c(89,91,94,96.75,99.5,101.25)
n1 <- length(T1)
n2 <- length(T2)
# It is assumed that the given measured values are a realization of
# of independent random variables $X_1, ..., X_4, Y_1, ..., Y_6$ and
# these random variables and these random variables have the
# continuous distribution function $F$ and $G$, respectively. Test
# the hypothesis $H_0 : F=G$ at the level $\alpha = 0.05$ by
# applying an appropriate nonparametric test.

# Determining the ranks
sample <- tibble(
  grp = c(rep("T1",n1), rep("T2",n2)),
  dur = c(T1,T2),
  rang = rank(dur)
)

# Determining of R.T1 and R.T2
sample %>% filter(grp == "T1") %>% summarise(sum(rang)) %>% 
  as.numeric() -> R.T1 # 24
sample %>% filter(grp == "T2") %>% summarise(sum(rang)) %>% 
  as.numeric() -> R.T2 # 31

# test statistic
U.T1 <- n1*n2 +n1*(1+n1)*0.5 - R.T1 # 10
U.T2 <- n1*n2 +n2*(1+n2)*0.5 - R.T2 # 14
t.xy <- (U -n1*n2*0.5)/sqrt(n1*n2*(n1+n2+1)/12)
t.xy

alpha <- 0.05
# Distribution of the Wilcoxon Rank Sum Statistic
qwilcox(c(alpha/2,1-alpha/2),n1,n2) # 3;21

# decision: H0: location shift = 0
(qwilcox(alpha/2,n1,n2) < U.T2 ) & (U.T2 < qwilcox(1-alpha/2,n1,n2)) # true -> no rejection
wilcox.test(T1,T2, alternative = "two.sided", paired = FALSE, 
            conf.level = 1-alpha) # p-value = 0.7612