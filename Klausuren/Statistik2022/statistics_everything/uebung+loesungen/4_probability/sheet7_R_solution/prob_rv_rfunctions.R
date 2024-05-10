#####################################################################
# R offers for a large number of probability distributions functions.
# The commands for each distribution are prepended with a letter to 
# indicate the functionality:
# "d" 	returns the height of the probability density function
# "p" 	returns the cumulative density function
# "q" 	returns the inverse cumulative density function (quantiles)
# "r" 	returns randomly generated numbers
# Consider an urn with 100 balls, where are 30 balls of them are red. 
# 20 balls are randomly drawn and let X be the number of red drawn balls. 
# a) Determine the distribution of X if the balls are drawn with resp.
# without replacement.
# b) Plot the density of X.
# c) Generate a sample of size 20 of values of X
# d) Compute P(5 < X < 15). 
# e) Determine the 25% quantile, the median and the 75% quantile of X.
# 
# file: prob_rv_rfunctions.R
########################################################################

# with replacement: X ~ B(n=20,p=0.3)

# plot of the density
k <- 0:20
plot(k,dbinom(k,20,0.3), type = "h", main ="B(n=20,p=0.3", xlab="x", 
     ylab="density")
# sample of size 20
rbinom(n=20,size = 20,prob = 0.3)
# P(5 < X < 15)
sum(dbinom(6:14, size = 20, prob = 0.3))
pbinom(14, size = 20, prob = 0.3) - pbinom(5, size = 20, prob = 0.3) 
# quantile
qbinom(c(0.25,0.5,0.75), size = 20, prob = 0.3)

# without replacement: X ~ H(n=20,M=30,N=100)

# plot of the density
k <- 0:20
plot(k,dhyper(k,m=30,n=70,k=20), type = "h", main ="H(n=20,M=30,N=100",
     xlab="x", ylab="density")
# sample of size 20
rhyper(20,m=30,n=70,k=20)
# P(5 < X < 20)
sum(dhyper(6:14,m=30,n=70,k=20))
phyper(14,m=30,n=70,k=20) - phyper(5,m=30,n=70,k=20)
# quantile
qhyper(c(0.25,0.5,0.75),m=30,n=70,k=20)
