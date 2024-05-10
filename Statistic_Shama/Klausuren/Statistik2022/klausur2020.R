#Klausur 2020
library(tidyverse)
library(DescTools)
library(psych)

r <- MASS::Cars93;head(r)
view(r)
scale <-  tibble(name = character(), type = character() , level = character())

scale1 <- scale %>% add_row(name = "Manufactor", type = "QN", level= "Nominal") %>% 
                    add_row (name = "Model", type = "QT", level= "Nominal") %>%
                    add_row (name = "Type", type = "QL", level= "Ordinal") %>%
                    add_row (name = "Price", type = "QT", level= "ratio") %>%  
                    add_row (name = "Cylinder", type = "QT", level= "absolute") %>%    
                    add_row (name = "Origin", type = "QL", level= "Nominal") %>%    
                    add_row (name = "RPM", type = "QT", level= "absolute") 
scale1



####Aufgabe 1b

mean(r$Price) 
quantile(r$Price, probs = c(0.25,0.5,0.75, type= 1))
boxplot(r$Price)
####Aufgbae 1c


# c <- r %>% select(Type, Origin)
# c
#  table <- table(r$Type, r$Origin)
# table
t1 <- r %>% select(Type,Origin)

tab <- table(t1)
tab
prop.table(table(t1$Type,t1$Origin))*100
chisq.test(t1$Type,t1$Origin)$expected

chisq.test(t1$Type,t1$Origin)$statistic



####Aufgbae 3a

xx <- tibble(x = c(1:5),prop= c(0.3,0.3,0.2,0.1,0.1))
xx # apply(data[ , c('a', 'c', 'd')], 2, sd)
mean <-tapply(xx$x,xx$prop)
mean
var <- var(mean)
sd(mean)
x = c(1:5)
prop= c(0.3,0.3,0.2,0.1,0.1)
s <- table(x,prop)
s
mean(s)
var(s)
sd(s)
mean (xx, trim = 0, na.rm= TRUE )
variance <- tapply(xx$x,xx$prop)
variance  sd(variance)

tapply(xx$x,xx$prop,median)
sd (xx$x)
var(240)


x <- c(0:10, 50)
x
xm <- mean(x)
c(xm, mean(x, trim = 0.10))

















































