##########################################################################
# Exercise: 2 paired sample t-test
# 
# file: infstat_2samples_testing_effect_alc.R
##########################################################################

# 2 dependent random variable with unknown but equal variances
# A (hypothetical) experiment is conducted on the effect of
# alcohol on perceptual motor ability. Ten subjects are each tested
# twice, once after having two drinks and once after having two
# glasses of water. The two tests were on two different days to give
# the alcohol a chance to wear off. Half of the subjects were given
# alcohol first and half were given water first. The scores of the 10
# subjects are shown below. The first number for each subject is their
# performance in the "water" condition. Higher scores reflect better
# performance. Test to see if alcohol had a significant effect. Report
# the t and p values. 

water <- c(16,15,11,20,19,14,13,15,14,16)
alc <-   c(13,13,10,18,17,11,10,15,11,16)

# H0: mu = 0, H1: mu <> 0
t.test(water, alc, alternative = "two.sided",
       mu = 0, paired = TRUE, var.equal = TRUE,
       conf.level = 0.95)

# reject H0, since p-value = 0.0007205