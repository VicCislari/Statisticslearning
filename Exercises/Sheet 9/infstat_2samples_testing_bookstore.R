##########################################################################
# Exercise: 1 sample t-test, 2 sample t-test, Welsh test, F-test, 
#           Wilcoxon-Mann-Whitney U-test
# 
# file: infstat_2samples_testing_bookstore.R
##########################################################################
library(tidyverse)

# Heumann, Schoemaker Aufgabe 10.3
# Christian decide to purchase the new CD Bruce Springstee. His
# first thought is to buy it online, via an online auction . He
# discovers that he can also buy the CD, without bidding at an
# auction, from the same online store. He also looks at the price at
# an internet book staore which was recommended to him by a
# friend. He notes down the following prices in Euro.

ibs <- 16.95
os <- tibble(noa = c(18.19, 16.98, 19.97, 16.98, 18.19, 15.99, 13.79, NA, 
                     15.90, 15.90, 15.90, 15.90, 19.97, 17.72),
             auc = c(10.50, 12.00, 9.54, 10.55, 11.99, 9.30, 10.59, 10.50, 
                     10.01, 11.89, 11.03, 9.52, 15.49, 11.02)
)
# Calculate and interpret the the arithmetic mean, variance,
# standard deviation and the coefficient of variation for the the
# online, both for the auction and non-auction.
os %>% summarise(
  mean.noa = mean(noa, na.rm = TRUE), mean.auc = mean(auc, na.rm = TRUE),
  var.noa = var(noa, na.rm = TRUE), var.auc = var(auc, na.rm = TRUE),
  sd.noa = sd(noa, na.rm = TRUE), sd.auc = sd(auc, na.rm = TRUE),
  var.coeff.noa = sd.noa/mean.noa, var.coeff.auc = sd.auc/mean.auc)
# It seems to be evident, that the mean auction prices are lower than mean non-
# auction prices. But the auction prices show a higher variability to the mean
# for the auction prices

# Test the hypothesis that the mean price at the online store
# (no auction) is unequal 16.95 Euro (alpha = 0.05).
alpha <- 0.05
t.test(x = os$noa, alternative = "two.sided", mu = ibs, conf.level = 1-alpha)
# One Sample t-test: t = 0.16237, df = 12, p-value = 0.8737
# conclusion: there is no evidence, that the price at the online store differ from 16.95

# Calculate a confidence interval for the mean price at the
# online store (no auction) and interpret your findings in the
# light of the hypothesis in b).
# 95 percent confidence interval:  15.96603 18.09243

# Test the hypothesis that the mean price at the online store
# (auction) is less than 16.95 Euro (alpha=0.05).
t.test(x = os$auc, alternative = "less", mu = ibs, conf.level = 1-alpha)
# One Sample t-test: t = -14.203, df = 13, p-value = 1.352e-09
# conclusion: the mean auction prices are lower than the price from the
# book store

# Test the hypothesis that the mean non-auction price is
# higher than the mean auction price. Assume (i) that the
# variances are equal in both samples and (ii) that the varainces
# are unequal.
# i) equal variances
t.test(x = os$noa, y = os$auc, alternative = "greater", paired = FALSE,
       var.equal = TRUE, conf.level = 1-alpha)
# Two Sample t-test: t = 9.4205, df = 25, p-value = 5.274e-10
# conclusion: mean auction prices are lower than the mean non-auction prices
# i) not equal variances
t.test(x = os$noa, y = os$auc, alternative = "greater", paired = FALSE,
       var.equal = FALSE, conf.level = 1-alpha)
# Welch Two Sample t-test: 9.3792, df = 24.123, p-value = 8.066e-10
# conclusion: same as before

# Test the hypothesis that the variance of the non
# auction-price is unequal to the variance of the auction price
# (alpha=0.05).
var.test(x = os$noa, y = os$auc, alternative = "two.sided", conf.level = 1-alpha)
# F test to compare two variances: F = 1.2577, num df = 12, denom df = 13, 
#                                  p-value = 0.6856
# conclusion: no evidence, that the variances are different; this justifies the 
# use of 2 sample t.test with equal variances. In practice, it is best to use the
# Welsh-test rather than the t-test

# Use the Wilcoxon-Mann-Whitney U-test to compare the location
# of the auction and non-auction prices.
wilcox.test(x = os$noa, y = os$auc, conf.level = 1-alpha)
# Wilcoxon rank sum test with continuity correction: W = 181, p-value = 1.347e-05
# conclusion: the locations are shifted


