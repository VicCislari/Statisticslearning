#####################################################
# Descriptive Statistics: Simple example linear regr.
# Complete Solution
#
# File: des_stat_ex_lin_reg_sol.R
#
#####################################################

# data
x <- c(2,6,3,4,5)
y <- c(3,7,4,7,6)

# scatterplot
par(mfrow=c(1,1))
plot(x,y,main="scatterplot", xlim=c(0,7), ylim = c(0,8))

# covariance
cov(x,y) # 2.5

# coefficient of correlation
cor(x,y) #  0.8703883

# regression line: y=a+bx
# criterion variable Y and predictor variable X
lm(y~x)
# a = 1.4, b = 1.0
a <- lm(y~x)$coefficients[1]
b <- lm(y~x)$coefficients[2]

# regression line: x = alpha + beta * y
# criterion variable X and predictor variable Y
lm(x~y)
# alpha = -0.0909, beta = 0.7576
alpha <- lm(x~y)$coefficients[1]
beta <- lm(x~y)$coefficients[2]

# transform the regression x = alpha + beta * x to y= a' + b' x
a_strich <- -alpha/beta
b_strich <- 1/beta

# gemeinsames Diagramm
plot(x,y,main="scatterplot",
     sub="regression: y ~ x (blue), x ~y (red)",
     xlim=c(1,7), ylim = c(0,8))
abline(lm(y~x), col="blue")
abline(a=a_strich, b=b_strich,col="red")

# eps-file
#dev.copy2eps(file="../pictures/ex_bi.eps")

# Diagramm mit ggplot()
library(tidyverse)
ggplot(data = tibble(x=x, y=y)) +
  geom_point(mapping = aes(x=x,y=y)) +
  geom_abline(slope = b, intercept = a, color = "blue") +
  geom_abline(slope = b_strich, intercept = a_strich, color = "red") +
  ggtitle("scatterplot", subtitle = "regression: y ~ x (blue), x ~y (red)") +
  theme_classic()