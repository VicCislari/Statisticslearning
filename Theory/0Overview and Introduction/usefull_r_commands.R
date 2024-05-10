#######################################################
# Usefull R Commands
#
# file: usefull_r_commands.R
#######################################################
library(tidyverse)

#######################################################
# sequence generation
#######################################################
# from:to
3:10
# c() - command
c(1,7:9)
c(1:5, 10.5, "next")
# seq()
seq(0, 1, length.out = 11)
seq(1, 9, by = 2)     # matches 'end'
seq(1, 6, by = 3)
seq(1.5, 2.1, by = 0.1)
# rep()
rep("abc", times = 3)
rep(1:4, times = 2)
rep(1:4,  times = c(2,3,1,2))
rep(c("a","b","c"), length.out = 5)
rep(1:4, length.out = 6)  


#######################################################
# Data: Conversion, Information, Manipulation
#######################################################
# as.integer(), as.numeric, as.character()
x <- pi * c(-1:1, 10)
x
x %>% as.integer()
c("-.123","2.7","312.3") %>% as.numeric()
as.numeric(c("-.123","2.7","312.3")) %>% as.character()
# is.na()
c("-2","45","AB") %>% as.integer()
c("-2","45","AB") %>% as.integer() %>% is.na()
# duplicated()
x <- c(1:4,seq(1,8,by=2))
x
x %>% duplicated()
# unique()
c(1:4,seq(1,8,by=2)) %>% unique()
# rev()
x <- c(1:5, 5:3)
x
rev(x)
# sort()
x <- c(1:5, 5:3)
sort(x)
sort(x, decreasing = TRUE)
# cut()
x <- c(1,2,3,4,5,2,3,4,5,6,7)
cut(x, breaks = 2)
cut(x, breaks = c(0,2,5,8))
cut(x, breaks = c(0,2,5,8),
    labels = c("class 1","class 2","class 3"))

#######################################################
# statistical functions
#######################################################
x <- c(sample(x = 1:5, size = 10, replace = TRUE),
       NA,NA,sample(x = 3:8, size = 5, replace = TRUE))
x
max(x, na.rm = TRUE)
min(x, na.rm = TRUE)
mean(x, na.rm = TRUE)
median(x, na.rm = TRUE)
sum(x, na.rm = TRUE)
var(x, na.rm = TRUE)
sd(x, na.rm = TRUE)
x[!is.na(x)]
x[!is.na(x)] %>% cumsum()
x[!is.na(x)] %>% rank()
x[!is.na(x)] %>% table()
table(x)
summary(x)
x[!is.na(x)] %>% quantile()
x[!is.na(x)] %>% quantile(probs = c(0.2,0.4,0.6,0.8))

#######################################################
# frequencies - one variable
#######################################################
sample_values <- 
  tibble(
    X = sample(x=c("a","b","c"), size = 10, 
               replace = TRUE),
    Y = round(runif(10,0,5))
  )
sample_values
# table()
table(sample_values$X)
table(sample_values$Y)
# ecdf()
H <- ecdf(sample_values$Y)
# values of H at all sample values
H(sample_values$Y)
# calculate values of H
H(c(-1,0.345,4,4.8,6))
# values of H at the sample values
H_tab <-
  sample_values %>%
  select (Y) %>%
  rename(x = Y) %>%
  mutate(H_x = H(x)) %>%
  unique() %>%
  arrange(x)
H_tab
# plot.ecdf implements the plot method for ecdf objects,
plot.ecdf(sample_values$Y, ylab = "H(x)",
          main = "empirical cumulative distribution function")

#######################################################
# frequencies - two variables
#######################################################
# create sample data - categorial variables
cat_data <-
  tibble(
    hair = sample(x=c("black","brown","red","blond"),
                  size = 20, replace = TRUE),
    sex = sample(x=c("male","female"), size = 20, 
                 replace = TRUE)
    )
cat_data
# table(x,y)
table(cat_data$hair, cat_data$sex)
# add marginal values
table(cat_data$hair) %>% addmargins()
table(cat_data) %>% addmargins()
# create sample data - cont. variables
cont_data <-
  tibble(
    x = round(runif(20,1,10)),
    y = round(runif(20,1,10))
  ) 
cont_data
# characteristic numbers
summary(cont_data)
# table()
table(cont_data)
# covariance
cov(cont_data$x,cont_data$y)
# coeff. of correlation
cor(cont_data$x,cont_data$y)
# histogram
hist(cont_data$x)
# empircial distribution function of the var. x
ecdf(cont_data$x)
# plot
plot.ecdf(cont_data$x)
# values at x
ecdf(cont_data$x)(cont_data$x)
# table values of H
dist_tab_x <-
  cont_data %>%
  select(x) %>%
  mutate(cum_rel_freq = ecdf(x)(x)) %>%
  arrange(x) %>%
  mutate(rel_freq = 
           if_else(row_number()==1,
                   cum_rel_freq,
                   cum_rel_freq - lag(cum_rel_freq))) %>%
  select(x,rel_freq,cum_rel_freq) %>%
  filter(!(rel_freq == 0))
dist_tab_x  <- 
  tibble(
    x = cont_data$x,
    H = ecdf(x)(x)
    ) %>%
  arrange(x) %>%
  unique()
  
# grouped values of x
dist_tab_x_g <- 
cut(cont_data$x, 
    breaks = c(0,0.2,0.4,0.5,0.6,0.8,1)) %>%
  table() %>%
  as.tibble() %>%
  mutate(H = cumsum(n)/length(cont_data$x))
  
# lm()
X <- cont_data$x
Y <- cont_data$y
reg <- lm(Y ~ X)
reg
# coefficients of the regression line
reg$coefficients
# table with x, y, fitted value and residual
cont_data %>%
  mutate(pred = reg$fitted.values,
         res = reg$residuals)

######################################################
# Descriptive statistics by groups
######################################################
# To compute summary statistics by groups, the functions
# group_by() and summarise() [in dplyr package] can be 
# used.
# Example: we want to group the built-in R data set 
# named iris by Species and then:
# compute the number of element in each group, compute 
# the mean and the standard deviation.
iris %>%
  group_by(Species) %>% 
  summarise(
    count = n(), 
    mean = mean(Sepal.Length, na.rm = TRUE),
    sd = sd(Sepal.Length, na.rm = TRUE)
  )

#######################################################
# sample()
#######################################################
x <- 1:12
# a random permutation
sample(x)
# sampling with replacement
sample(x, replace = TRUE)
# 5 Bernoulli trials
sample(c("tail","head"), 5, replace = TRUE)
# sampling with replacement out of an urne where
# 50% of the balls are red, 20% of the balls are white
# and 30% of the balls are blue
sample(c("red","white","blue"), size = 10,
       replace = TRUE,
       prob = c(0.5,0.2,0.3))
# sampling without replacement out of an urne with
# 5 red balls, 2 white balls and 3 blue balls
sample(c("red","red","red","red","red",
         "white","white",
         "blue","blue","blue"), 
       size = 7, replace = FALSE)
