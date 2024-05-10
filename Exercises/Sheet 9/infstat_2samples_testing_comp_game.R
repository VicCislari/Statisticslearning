##########################################################################
# Exercise: Welsh test, Wilcoxon-Mann-Whitney U-test
# 
# file: infstat_2samples_testing_comp_game.R
##########################################################################
library(tidyverse)

# Heumann, Schoemaker Aufgabe 10.6}

# Two friends play a computer game and each of them repeats the same
# level 10 times. The score obtained are:
  
P1 <- c(91,101,112,99,108,88,99,105,111,104)
P2 <- c(261,47,40,29,64,6,87,47,98,351)

# Player 2 insists that he is a better player and suggests to
# compare their performance. Use an appropriate test
# (alpha=0.05$) to test this hypothesis.
# 2 sample Welsh test
alpha <- 0.05
t.test(x = P1, y = P2, alternative = "less", paired = FALSE, 
       var.equal = FALSE, conf.level = 1-alpha)
# p-value = 0.4869
# conclusion: no evidence to that P2 is better than P1

# Player 1 insists that he is a better player. He propose to
# not focus on the mean and to use the Wlicoxon-Mann-Whitney
# U-test for comparison (alpha=00.5). What are the advantages
# and disadvantages of using this test compared with a)?
wilcox.test(x = P1, y = P2, alternative = "greater", conf.level = 1-alpha)
# p-value = 0.01875
# conclusion: evidence that P1 is better than P2
# The U-test has the advantage of not being focused to the mean. The two
# sample are clearly different: P2 scores with much more variability and 
# his distribution is not symmetric and normally distributed. Since the
# sample is small, and the assumption of a normal distribution is likely 
# not met, it makes no sense to use a t-test. Moreover, because the
# distribution is skewed the mean may not be a sensible measure of
# comparison. A drawback of the U-test is that it uses only the ranks and 
# not the raw data: it thus uses less informaton than the the t-test, 
# which would be preferred when comparing means of a reasonably sized sample.

