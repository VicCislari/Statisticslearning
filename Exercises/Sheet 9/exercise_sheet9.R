# sheet 9

# task 1: 2 sample Gauss Test
x <- c(5.46, 5.34, 4.34, 4.82, 4.4, 5.12, 5.69, 5.53, 4.77, 5.82)
y <- c(5.45, 5.31, 4.11, 4.69, 4.18, 5.05, 5.72, 5.54, 4.62, 5.89, 5.6, 5.19,
       3.31, 4.43, 5.3, 4.09)
mean.x <- mean(x)
mean.y <- mean(y)
n.x <- length(x)
n.y <- length(y)
sigma.x <- 0.5
sigma.y <- 0.6

test.statistic <- (mean.x - mean.y)/sqrt(sigma.x^2/n.x + sigma.y^2/n.y)
# decisoon rule: reject H0 if
test.statistic < qnorm(0.05)
p.value <- pnorm(test.statistic)
p.value

# task 2
sample.1 <- c(7.06, 11.84, 9.28, 7.92, 13.5, 3.98, 3.82, 7.34, 8.7, 9.24, 4.86, 3.32,
              12.78, 12, 5.24, 11.4, 6.56, 9.04, 7.72, 9.26, 7.88, 8.6, 9.3, 8.42, 8.54)
sample.2 <- c(8.68, 6, 6.3, 10.24, 10.88, 5.36, 7.82, 4.7, 9.02, 9.78, 6.9, 5.8, 13.56,
              10.32, 13.3, 11.38, 7.94, 10.74, 13.68, 14.92, 7.42, 10.36, 10.54,
              5.22, 13.74, 12.98, 10.34, 10.02, 17.8, 13.04, 5.2, 9.4, 11.18, 12.68,
              12.36)
# H0: mu1 >= mu2, H1: mu1 < mu2
alpha <- 0.05
# check whether the variances are equal or not
var.test(x=sample.1, y=sample.2, ratio = 1, alternative = "two.sided",
         conf.level = 0.9)
# F test to compare two variances
# 
# data:  sample.1 and sample.2
# F = 0.73589, num df = 24, denom df = 34, p-value = 0.4377
# alternative hypothesis: true ratio of variances is not equal to 1
# 90 percent confidence interval:
#   0.3993547 1.4108015
# sample estimates:
#   ratio of variances 
# 0.7358912 
# decision: The H0 that both variances are equal can not be rejected, since the 
# pvalue is bigger than 0.1!
# => Assumption both variances are equal, i.e conduct a t test

# H0: mu1 >= m2, H1: mu1 < mu2
t.test(x=sample.1, y=sample.2, mu=0, alternative = "less", var.equal = TRUE,
       paired = FALSE, conf.level = 1-alpha)
# Two Sample t-test
# 
# data:  sample.1 and sample.2
# t = -2.1444, df = 58, p-value = 0.0181
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#   -Inf -0.3714405
# sample estimates:
#   mean of x mean of y 
# 8.304000  9.988571 
# Decision: reject H0

# task 3
# H0: mu1 = mu2, H1: mu1 <> mu2
# 2 sample paired test
sample.water <- c(16, 15, 11, 20, 19, 14, 13, 15, 14, 16)
sample.alc <- c(13, 13, 10, 18, 17, 11, 10, 15, 11, 16)
t.test(x=sample.water, y=sample.alc, alternative = "two.sided", paired = TRUE, 
       conf.level = 0.95)
# Paired t-test
# 
# data:  sample.water and sample.alc
# t = 5.0186, df = 9, p-value = 0.0007205
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   1.043561 2.756439
# sample estimates:
#   mean of the differences 
# 1.9 
# decision: reject H0, since the pvalue is lower than 0.05; i.e. alcohol seems
# to have an effect

# task 4
sample.a <- c(102.4, 101.3, 97.6, 98.2, 102.3, 99.1, 97.8, 103.9, 101.6, 100.1)
sample.b <- c(98.4, 101.7, 100.5, 99.3, 100.6, 99.6, 102.2, 101.1, 99.9, 101.0)
# H0: sigma.a <= sigma.b, H1: sigma.a > sigma.b
var.test(x=sample.a, y=sample.b, alternative = "greater", conf.level = 0.95, ratio = 1)
# F test to compare two variances
# 
# data:  sample.a and sample.b
# F = 3.634, num df = 9, denom df = 9, p-value = 0.03404
# alternative hypothesis: true ratio of variances is greater than 1
# 95 percent confidence interval:
#   1.143167      Inf
# sample estimates:
#   ratio of variances 
# 3.634007 
# decision: reject H0, since pvalue is < alpha=0.05, i.e. scale B seems to be better
# than scale A

# task 5
sample.a <- c(7.2, 4.1, 5.5, 4.5, 5.7, 3.8, 4.6, 6.0, 5.2, 5.4)
sample.b <- c(5.3, 4.4, 5.0, 3.5, 3.9, 4.9, 5.6, 2.5, 4.0, 3.6)

# a) H0: sigma.a = sigma.b, H1: sigma.a <> sigma.b
var.test(x=sample.a, y=sample.b, alternative = "two.sided", 
         conf.level = 0.9, ratio = 1)
# F test to compare two variances
# 
# data:  sample.a and sample.b
# F = 1.1077, num df = 9, denom df = 9, p-value = 0.8814
# alternative hypothesis: true ratio of variances is not equal to 1
# 90 percent confidence interval:
#   0.3484569 3.5212834
# sample estimates:
#   ratio of variances 
# 1.107707
# decision: H0 can not be rejected, since pvalue > .1; the carainces can
# be assumed to be equal

# b) H0: mua <= mub, H1: mua > mu.b
t.test(x=sample.a, y=sample.b, alternative = "less", var.equal = TRUE,
       mu=0, conf.level = 0.975)
# Two Sample t-test
# 
# data:  sample.a and sample.b
# t = 2.1273, df = 18, p-value = 0.02374
# alternative hypothesis: true difference in means is greater than 0
# 97.5 percent confidence interval:
#   0.01152883        Inf
# sample estimates:
#   mean of x mean of y 
# 5.20      4.27 
# decision: reject H0, since pvalue < 0.025, i.e. A seems to be better
# than B

# task 14
library(tidyverse)

# import the data
data <- read_csv2(file = "magnets_pain.csv")
data

# side by side boxplot
boxplot(data$Score_2~data$Active,
        names = c("Treatment", "Control"),
        xlab = "", ylab = "Pain rating")

# characteristic numbers
data %>%
  group_by(Active) %>%
  summarise(Mean = mean(Score_2), Min = min(Score_2), Max = max(Score_2),
            Q1 = quantile(Score_2, 0.25), Q2 = quantile(Score_2, 0.5),
            Q3 = quantile(Score_2, 0.75), IQR = Q3-Q1, SD = sd(Score_2))
# It seems to be that the treatment reduces pain

# compare the variances: H0: sigma.treatment <= sigma.control
treatment <-  data %>% filter(Active == 1) %>% 
  select(Score_2) %>% as_vector()
control <-  data %>% filter(Active == 2) %>% 
  select(Score_2) %>% as_vector()

var.test(x=treatment, y=control, alternative = "greater", conf.level = 0.95)
# F test to compare two variances
# 
# data:  treatment and control
# F = 2.8598, num df = 28, denom df = 20, p-value = 0.008893
# alternative hypothesis: true ratio of variances is greater than 1
# 95 percent confidence interval:
#   1.393891      Inf
# sample estimates:
#   ratio of variances 
# 2.859789 
# decision: reject H0, since pvalue < 0.05

# => The variances must be assumed to be unequal.

# H0: mu.treatment >= mu.control; H1: mu.treatment < mu.control
t.test(x=treatment, y=control, alternative = "less", mu=0, var.equal = FALSE,
       conf.level = 0.95)
# Welch Two Sample t-test
# 
# data:  treatment and control
# t = -5.695, df = 46.418, p-value = 4.029e-07
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#   -Inf -2.855924
# sample estimates:
#   mean of x mean of y 
# 4.379310  8.428571 

# decision: since pvalue < alpha=0.5, H0 can be rejected