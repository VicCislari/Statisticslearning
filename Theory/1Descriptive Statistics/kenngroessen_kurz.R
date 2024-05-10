##############################################################
# Example: characteristic numbers and diagrams
#          univariate and bivariate data
#
# file: kenngroessen_kurz.R
##############################################################
library(tidyverse)

x <- rnorm(n=20, mean = 10, sd = 4) %>% round()
y <- rnorm(n=20, mean = 10, sd = 4) %>% round()
x; y


# mean
mean(x)
# quantiles
quantile(x, probs = c(0.25,0.5,0.75))
# boxplot
boxplot(x)
# barplot
barplot(x)
# histogram
hist(x)

# trimmed mean
mean(x, trim = 0.05) # 10% trimmed mean

# variance
var(x)
# standard deviation
sd(x)

# covariance
cov(x,y)
# correlation coefficient
cor(x,y)
# spearman 
cor(x,y, method = "spearman")

# scatterplot
plot(x,y)

# linear regression
reg1 <- lm(y~x) # y = b*x + a
reg2 <- lm(x~y) # x = beta*y + alpha
# paramters regression line
reg1$coefficients
a1 <- reg1$coefficients[1]
b1 <- reg1$coefficients[2]
reg2$coefficients
a2 <- -reg2$coefficients[1]/reg2$coefficients[2]
b2 <- 1/reg2$coefficients[2]

# scatter and regression lines
plot(x,y)
abline(lm(y~x), lwd=2)
abline(a=a2, b=b2, lwd=2)
points(x=x[1], y=y[1], col="red", lwd=2)
segments(x0=x[1], y0=y[1], x1= x[1], y1=reg1$fitted.values[1], col = "blue", lwd=2)
segments(x0=x[1], y0=y[1], x1= reg2$fitted.values[1], y1=y[1], col = "black", lwd=2)

# residuals
reg1$residuals
reg2$residuals
# fitted values
reg1$fitted.values
reg2$fitted.values


# contingency table
gender <- sample(c("f","m"), 20, replace = TRUE)
grades <- sample(1:5, 20, replace = TRUE)
gender; grades
table(gender,grades) %>% addmargins()

res <- chisq.test(gender, grades)
res$observed  # contingency table
res$expected  # indifference table
res$statistic # chi square

res$expected  # indifference table