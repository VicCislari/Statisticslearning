# Sheet 1: Introduction to R, RStudio
# WS 2019/20

# Task 1: 
# sum of 52.3, 74.8, 3.17
52.3+74.8+3.17
# the square root of 144
144**0.5
# the 10-based logarithm of 200 multiplied with sin of $\pi/4$
log10(200)*sin(pi/4)
# the cumulative sum of the numbers 1,3,18,20,2 
cumsum(c(1,3,18,20,2))
# find 10 numbers between 0 and 20 rounded to the nearest
sample(x = 0:20, size = 10, replace = FALSE)
# or
round(runif(n = 10, min = 0, max = 20))

# Task 2: 
x <- 5
y <- 10
x * y
z <- x * y
myvec <- c(x,y,z)
min(myvec)
max(myvec)
mean(myvec)
rm(myvec)
  
# Task 3
rainfall <- c(0.1,0.5,2.3,1.1,11.3,14.7,23.4,15.7,0,0.9)

# a) 
mean(rainfall)
sd(rainfall)

# b)
cumsum(rainfall)

# c)
sum(rainfall)

# d)
which.max(rainfall)

# e)
rainfall[rainfall>10]

# f)
mean(rainfall[rainfall >=5])

# f)
rainfall[rainfall == 0 | rainfall == 1.1]
# alternative solution
rainfall[rainfall %in% c(0,1.1)]
# days where the rainfall is 0 or 1.1
which(rainfall %in% c(0,1.1))

# Task 4:
len <- c(2.5, 3.4, 4.8, 3.1, 1.7)
diam <- c(0.7, 0.4, 0.5, 0.5, 0.9)
 # a)
vol <- len * diam**2 * pi
vol

# b)
vol.cm <- 10*len * (10*diam)**2 * pi
vol.cm

# Task 5: 
x <- c(1,2,3,4,5)
y <- c(3,5,7,9)
# a)
intersect(x,y)

# b)
# x without y
setdiff(x,y)
# y without x
setdiff(y,x)

# c)
union(x,y)
# c(x,y) only concatenates x and y
c(x,y)

# Task 6
mat1 <- matrix(c(seq(0,18, by = 2), 
                 as.integer(runif(70,0,100))), 
               nrow = 8, ncol = 10, byrow = TRUE)
mat1
# a)
rm <- rowMeans(mat1)
rm
sd(rm)

# b)
# removing the first row of mat1 
mat2 <- mat1[-2,]
mat2
# colum means of mat2
cm <- colMeans(mat2)
cm
# creating a histogram of cm
hist(cm)

# Task 7
library(ggplot2)
library(tidyverse)
# a) 
names(mpg)
head(mpg)

# b) description of the dataset
help(mpg)

# c)
str_mpg <- data_frame(name = names(mpg),
                      type = rep(NA,length(names(mpg))),
                      level = rep(NA,length(names(mpg))),
                      dc = rep(NA,length(names(mpg))))
# properties of the variables
str_mpg[str_mpg$name =="manufacturer",2:4] <- c("qualitative","nominal","discrete")
str_mpg[str_mpg$name =="model",2:4] <- c("qualitative","nominal","discrete")
str_mpg[str_mpg$name =="displ",2:4] <- c("quantitative","ratio","continous")
str_mpg[str_mpg$name =="year",2:4] <- c("quantitative","interval","discrete")
str_mpg[str_mpg$name =="cyl",2:4] <- c("quantitative","ratio","discrete")
str_mpg[str_mpg$name =="trans",2:4] <- c("qualitative","nominal","discrete")
str_mpg[str_mpg$name =="drv",2:4] <- c("qualitative","nominal","discrete")
str_mpg[str_mpg$name =="cty",2:4] <- c("quantitative","ratio","continous")
str_mpg[str_mpg$name =="hwy",2:4] <- c("quantitative","ratio","continous")
str_mpg[str_mpg$name =="fl",2:4] <- c("qualitative","nominal","discrete")
str_mpg[str_mpg$name =="class",2:4] <- c("qualitative","nominal","discrete")
# display the 
str_mpg

# alternative solution

# create an empty tibble
str_mpg1 <- tibble(name = character(), type = character(), level = character(), dc = character())
# add rows
str_mpg1 <- str_mpg1 %>%
  add_row(name = "manufacturer", type = "qualitative", level = "nominal", dc = "discrete") %>%
  add_row(name = "model", type = "qualitative", level = "nominal", dc = "discrete") %>%
  add_row(name = "displ", type = "quantitative", level = "ratio", dc = "continous") %>%
  add_row(name = "year", type = "quantitative", level = "interval", dc = "discrete") %>%
  add_row(name = "cyl", type = "quantitative", level = "ratio", dc = "discrete") %>%
  add_row(name = "trans", type = "qualitative",level = "nominal", dc = "discrete") %>%
  add_row(name = "drv", type = "qualitative", level = "nominal", dc = "discrete") %>%
  add_row(name = "cty", type = "quantitative", level = "ratio", dc = "continous") %>%
  add_row(name = "hwy", type = "quantitative", level = "ratio", dc = "continous") %>%
  add_row(name = "fl", type = "qualitative", level = "nominal", dc = "discrete") %>%
  add_row(name = "class", type = "qualitative", level = "nominal", dc = "discrete")
head(str_mpg1)

# d)
subset(str_mpg, subset = (type == "quantitative" & dc == "discrete"))
