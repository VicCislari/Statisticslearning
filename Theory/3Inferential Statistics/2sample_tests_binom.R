##########################################################################
#
# Examples for two sample tests in the binomial model
#
# file: 2sample_tests_binom.R
##########################################################################

##########################################################################
# comparison the values of two independent (B1,p)-distributed random 
# variables
##########################################################################
# Example: Heumann, Schoemaker p. 231
# example slide 26
n.a <- 63
winning.a <- 14
n.b <- 45
winning.b <- 13
# estimations of p.a, p.b
p.a.est <- winning.a/n.a
p.b.est <- winning.b/n.b
d.est <- p.a.est - p.b.est
# HO: p.a = p.b, H1: p.a <> p.b
p.est <- (winning.a + winning.b) / (n.a + n.b)
alpha <- 0.05

# approximate test
# teststatistic
t.stat <- d.est/(p.est*(1-p.est)*(1/n.a + 1/n.b))^0.5
# reject H0, if |t.stat| >= (1-alpha/2) quantile of N(0,1)
qnorm(1-alpha/2)
abs(t.stat) >= qnorm(1-alpha/2) # no rejection
# p-value
pnorm(-abs(t.stat))+(1-pnorm(abs(t.stat)))

# exact fisher-test
cont.tab <- matrix(c(winning.a,(n.a-winning.a),winning.b,(n.b-winning.b)),
                   nrow = 2, ncol = 2, byrow = TRUE)
fisher.test(x = cont.tab, alternative = "two.sided", conf.level = 1-alpha)
# 	Fisher's Exact Test for Count Data
#
#data:  cont.tab
#p-value = 0.5014
#alternative hypothesis: true odds ratio is not equal to 1
#95 percent confidence interval:
#  0.2675788 1.8653917
#sample estimates:
#  odds ratio 
#0.7056471 
