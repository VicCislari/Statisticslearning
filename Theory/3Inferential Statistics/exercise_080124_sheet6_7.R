#Aufgabe 5)
pnorm(2, mean = 1.8, sd = sqrt(0.3^2+0.2^2))
1-pnorm(4, mean = 4.8, sd = 1)

#Aufgabe 1) ZGW-Satz
#a)
p <- 1-0.99*0.99*0.95*0.98
EX <- 1000*p
VarX <- 1000*p*(1-p)
#b)
pnorm(110+0.5, mean = EX, sd = sqrt(VarX))
pbinom(110, size = 1000, prob = p)
#c) 

p.neu <- 1-0.99^3*0.98
delta <- 100*(p-p.neu)

# Aufgabe 3 ZGW Satz
p <- c(0.2,0.7,0.1)
x <- c(0,1,2)
EX <- 1000*sum(p*x)
VarX <- 1000*(sum(p*x^2)-sum(p*x)^2)
A <- qnorm(0.99, mean = EX, sd = sqrt(VarX))

# Konfidenzintervalle
# Aufgabe 2
x <- c(8, 9, 10, 13, 14, 16, 17, 20, 21)
n <- length(x)
sigma <- 2.8
mw <- mean(x)
# a)
u <- mw-qnorm(0.975)*sigma/sqrt(n)
o <- mw+qnorm(0.975)*sigma/sqrt(n)
u; o
library(TeachingDemos)
z.test(x, alternative = "two.sided", conf.level = 0.95, sd = sigma)
# b)
u <- mw-qnorm(0.995)*sigma/sqrt(n)
o <- mw+qnorm(0.995)*sigma/sqrt(n)
u; o
library(TeachingDemos)
z.test(x, alternative = "two.sided", conf.level = 0.99, sd = sigma)

# Aufgabe 3
n <- 22
mw <- 60
alpha <- 0.01

#a)
sigma <- 10
u <- mw-qnorm(1-alpha/2)*sigma/sqrt(n)
o <- mw+qnorm(1-alpha/2)*sigma/sqrt(n)
u; o
# b)
s <- 10
u <- mw-qt(1-alpha/2, df=n-1)*s/sqrt(n)
o <- mw+qt(1-alpha/2, df=n-1)*s/sqrt(n)
u; o

# Aufgabe 4
alpha <- 0.05
x <- c(247.4, 249.0, 248.5, 247.5, 250.6, 252.2, 253.4, 248.3, 251.4, 246.9,
       249.8, 250.6, 252.7, 250.6, 250.6, 252.5, 249.4, 250.6, 247.0, 249.4)
# a) Konfidenzintervall für mu
sigma <- 2
u <- mean(x)-qnorm(1-alpha/2)*sigma/sqrt(length(x))
o <- mean(x)+qnorm(1-alpha/2)*sigma/sqrt(length(x))
u; o
#alternative Lösung
z.test(x, alternative = "two.sided", conf.level = 0.95, sd=sigma)

# b) Konfidenzintervall für mu, sigma unbekannt
n <- length(x)
s <- sd(x)
u <- mean(x)-qt(1-alpha/2, df=n-1)*s/sqrt(n)
o <- mean(x)+qt(1-alpha/2, df=n-1)*s/sqrt(n)
u; o
# alternative Lösung
t.test(x, alternative = "two.sided", conf.level = 1-alpha)

#c) Konfidenzintervall für sigma^2
mu <- 250
Qn <- sum((x-mu)^2)
u <- Qn/qchisq(1-alpha/2, df = length(x))
o <- Qn/qchisq(alpha/2, df = length(x))
u; o

# d) Konfidenzintervall für sigma^2, mu unbekannt
u <- (length(x)-1)*var(x)/qchisq(1-alpha/2, df = length(x)-1)
o <- (length(x)-1)*var(x)/qchisq(alpha/2, df = length(x)-1)
u;o
# alternative Lösung
library(TeachingDemos)
sigma.test(x, alternative = "two.sided", conf.level = 1-alpha)
