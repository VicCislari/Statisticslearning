##########################################################################
# Exercise: exact Fisher test and normal approximation
# 
# file: infstat_2samples_testing_tshirts.R
##########################################################################
# Heumann, Schoemaker Aufgabe 10.5

# A company producing clothing often finds deficient T-shirts among
# its production.
# The company's controlling department decides that the production is no longer
# profitable when there are more than 10% deficient T-shirts. A sample of 230 
# shirts yield 35 shirts which contain deficiencies. Use the approximate 
# binomial test and the exact binomial test to decide whether the shirt 
# production is profitable or not (alpha=0.05). 
n <- 230; def <- 31

# H0: p <= 0.1, H1: p > 0.1
p0 <- 0.1; r <- def/n
# one sample test for p: normal approximation
# test statistics
t.x <- (r-p0)/sqrt(p0*(1-p0)/n)
# decision: reject H0, if t.x > 1-alpha quantile of the N(0,1)-distr.
alpha <- 0.05
t.x > qnorm(1-alpha)
# rejection of H0

# one sample test for p: exact binomial test
binom.test(x = def, n = n, p = p0, 
           alternative = "greater", conf.level = 1-alpha)
qbinom(1-alpha, size = n, prob = p0) # 95% quantile of B(n,p0) is 31
# no rejection of H0, since p-value = 0.05414
# Since the exact binomial test is more precise than the approximate binomial
# test , we follow the result of the exact binomila test.

# The company is offered a new cutting machine. To test whether the change of 
# machine helps to improve the production quality, 115 sample T-shirts are 
# evaluated, 7 of which have deficiencies. Use the 2 sample binomial test to 
# decide whether the new machine yields improvement or not (alpha =0.05)

n1 <- 230; def1 <- 30
n2 <- 115; def2 <- 7
phat <- (def1+def2)/(n1+n2)

# H0: p2 >= p1, H1: p2 < p1

# two sample test for p: normal approximation
# test statistics
T <- (def2/n2 - def1/n1)/sqrt(phat*(1-phat)*(1/n1 + 1/n2))
T
# decision: reject H0, if T < alpha quantile of N(0,1)
T < qnorm(alpha) # rejection of H0

# two sample test for p: fisher's exact test
# contingency table:      machine2 machine1
#                    def      7     31
#                    ok       109   199
cont.tab <- matrix(c(def2,def1,n2-def2,n1-def1),
                   nrow = 2, ncol = 2, byrow = TRUE)
fisher.test(cont.tab, alternative = "less", conf.level = 1-alpha)
# rejection of H0, since p-value = 0.02465