# a simple r script

# sample: weight and height of males
weight <- c(75,81.5,72,79.5,85.5,65,77)
height <- c(180,187,179,189,190,166,183)
# means
mean(weight)
mean(height)
# diagram
plot(height,weight)

# include library
library(datasets)
data(women)
# average heights and weights for American women aged 30-39
women

weight