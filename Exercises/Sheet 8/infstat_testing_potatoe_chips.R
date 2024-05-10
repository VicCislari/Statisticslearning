###################################################################
# A bag of potato chips of a certain brand has an advertised weight
# of 250 grams. Actually, the weight (in grams) is a random variable. 
# Suppose that a sample of 81 bags has mean 248 and standard 
# deviation 5. At the 0.05 significance level, conduct the following 
# tests and calculate the p-values.
# a) H_0: mu >= 250 versus H_1: mu < 250
# b) H_0: sigma >= 7 versus H_1: sigma < 7
# file: infstat_testing_potatoe_chips.R
####################################################################

n <- 81
alpha <- 0.05
mean.sample <- 248
sd.sample <- 5
mean.0 <- 250
sd.0 <- 7

# a) t-test
tstat_a <- (mean.sample-mean.0)*sqrt(n)/sd.sample
tstat_a # -3.6
qt(1-alpha,n-1) # 1.664125
pvalue_a <- pt(tstat_a, df = n-1)
pvalue_a # 0.0002750739
  
# b)
tstat_b <- (n-1)*sd.sample^2/sd.0^2
tstat_b # 40.81633
qchisq(alpha,n-1) # 60.39148
pvalue_b <- pchisq(tstat_b, df = n-1)
pvalue_b # 8.081861e-05