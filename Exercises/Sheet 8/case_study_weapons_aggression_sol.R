###################################################################################
# Weapons and Aggression: t-test
# Case Study: https://onlinestatbook.com/case_studies_rvls/guns/index.html
#
#
# file: case_study_weapons_aggression_sol.R
###################################################################################

library(tidyverse)

# Background: The "weapons effect" is the finding that the presence of a weapon 
# or even a picture of a weapon can cause people to behave more aggressively. 
# One explanation of the weapons effect is that because guns have been associated 
# with aggression, seeing a gun increases the accessibility of associated aggressive 
# thoughts which in turn facilitate aggressive behavior. The idea that activation 
# of a concept in semantic memory increases activation and therefore accessibility 
# of related concepts is called spreading activation.If this spreading activation 
# explanation of the weapons effect is correct, then the presence of a weapon 
# word (such as "dagger" or "bullet") should increase the accessibility of an 
# aggressive word (such as "destroy" or "wound"). The accessibility of a word 
# can be measured by the time it takes to name a word presented on computer screen. 

# Experimental Design: The hypothesis is that a person can name an aggressive word 
# more quickly if it is preceded by a weapon word than if it word is preceded by 
# a neutral word. Each subject named both aggressive and non-agressive words 
# following both weapon and non-weapon "primes."
# The subjects were undergraduate students ranging in age between 18 and 24 years.
# They were told that the purpose of this study was to test reading ability of 
# various words. On each of the 192 trials, a computer presented a priming stimulus 
# word (either a weapon or non-weapon word) for 1.25 seconds, a blank screen for 
# 0.5 seconds, and then a target word (agressive or nonagressive word). The 
# experimenter instructed the subjects to read the first word to themselves and 
# then to read the second word out loud as quickly as they could. The computer 
# recorded reaction times. The means of the times in each of the four conditions 
# AN: Aggressive target word, nonweapon prime
# AW: Aggressive target word, weapon prime
# CN: Control target word (nonaggressive) and nonweapon prime
# CW: Control target word (nonaggressive) and weapon prime
# were used as the dependent variables. Therefore, each subject provided four 
# scores to the analysis.

# Load the raw data and make the data tidy if necessary. 
raw.data <- read.csv2(
  "C:/Users/Egbert Falkenberg/Nextcloud/NextCloud Lehre/aktuelle_LV/Statistik/Exercises/R_Solutions/Sheet_8/weapons.aggression.csv", 
         sep=";", comment.char="#")
raw.data

# make the data tidy
raw.data %>%
  gather(aw:cxen, key = "exp.setting", value = "time") -> tidy.data
tidy.data

# Generate side by side boxplots to visualize the results for the different experiments.
boxplot(tidy.data$time ~ tidy.data$exp.setting,
        names = c("AN", "AW", "CN", "CW"),
        ylab = "", xlab = "")
# Comparing the boxplots what is the effect of a preceded weapon?
# Answer: Notice that it took less time to name an aggressive target word when it was 
# preceded by a weapon prime (AW) then when it was preceded by a non-weapon 
# prime (AN). A comparison of the CW and CN conditions reveals no evidence of a 
# weapon prime for the nonaggressive target words.

# Evaluate the number of observations, min, max, mean and sd for all experimental settings.
tidy.data %>%
  group_by(exp.setting) %>%
  summarise(N = n(), Min = min(time), Max = max(time), Mean = mean(time), 
            Q1 = quantile(time, probs = 0.25), Q2 = quantile(time, probs = 0.5),
            Q3 = quantile(time, probs = 0.75), SD = sd(time))

# The hypothesis is that a person can name an aggressive word more quickly if it
# is preceded by a weapon word than if the aggressive word is preceded by a 
# neutral word. 
# Compute the difference between the mean naming time of aggressive words when 
# preceded by a neutral word and the mean naming time of aggressive words when 
# preceded by a weapon word separately for each subject. This difference score 
# (prime_agg) will be called the "aggressive-word priming effect." The hypothesis 
# would be supported if: the mean aggressive-word priming effect were positive.
# Evaluate prime_agg and the min, max, mean, Q1, Q2, Q3 and standard deviation of 
# prime_agg
prime_agg <- raw.data$an - raw.data$aw
tibble(diff = prime_agg) %>%
  summarise(N = n(), Min = min(diff), Max = max(diff), Mean = mean(diff), 
            Q1 = quantile(diff, probs = 0.25), Q2 = quantile(diff, probs = 0.5),
            Q3 = quantile(diff, probs = 0.75), SD = sd(diff))

# As hypothesized, the mean aggressive-word priming effect is positive. 

# Regard now the possibility that "weapon words" prime non-aggressive as well as
# aggressive words. To control for this, compute the difference between 
# (a) the mean naming time of non-aggressive words when preceded by a non-weapon word 
# and 
# (b) the mean naming time of non-aggressive words when preceded by a weapon word 
# separately for each subject. This difference represents the how much preceding 
# a non-aggressive word by a weapon word decreases the time it takes to name the 
# nonaggressive word and is called the "non-aggressive word priming effect." 
# Finally, for each subject, subtract this "non-aggressive word priming effect" 
# from the "aggressive-word priming effect" described above. This difference 
# will be called "prime difference" and labeled "prime_diff".
# Evaluate prime_diff and the min, max, mean, Q1, Q2, Q3 and standard deviation of 
# prime_diff

prime_diff <-
  # aggressive-word priming effect
  prime_agg - 
  # non-aggressive word priming effect
  (raw.data$cxen - # mean naming time of non-aggressive words when preceded 
                   # by a non-weapon word 
  raw.data$cxew)   # mean naming time of non-aggressive words when preceded by 
                   # a weapon word

tibble(diff = prime_diff) %>%
  summarise(N = n(), Min = min(diff), Max = max(diff), Mean = mean(diff), 
            Q1 = quantile(diff, probs = 0.25), Q2 = quantile(diff, probs = 0.5),
            Q3 = quantile(diff, probs = 0.75), SD = sd(diff))

# The hypothesis is that the mean prime difference will be positive.

# As shown before, the mean aggressive-word priming effect was 0.721. Check if
# this value differs significantly different from 0. 

t.test(x = prime_agg, alternative = "two.sided", mu = 0)

# Since p-value = 0.0323 we can support the hypothesis that preceding an 
# aggressive word with a weapon prime decreases the time it takes to name the 
# aggressive word.

# More important is the question that the"prime difference" score mean of 0.8432 
# differs significantly from zero. 

res.t.test.primed.diff <- t.test(x = prime_diff, alternative = "two.sided", mu = 0)
res.t.test.primed.diff

# Therefore, it can be concluded that weapon words prime aggressive words more 
# than they prime non-aggressive words.