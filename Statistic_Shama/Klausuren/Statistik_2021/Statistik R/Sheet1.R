number<-c(52.3,74.8,3.17)
sum(number)
sqr(144)
log(200,10)*sin(pi/4)
number2<-c(1,2,18,20,2)
cumsum(c(1,2,18,20,2))
round(sample(0:20,10))
round(runif(10,0,20))


#Aufgabe2

x<-5
y<-10

z<-(x*y)

myvec<-c(x,y,z)
max(myvec)
min(myvec)
rm(myvec)


#Aufgabe3

vector1<-c(0.1,0.5,2.3,1.1,11.3,14.7,23.4,15.7,0,0.9)
mean(vector1)
sd(vector1)
cumsum(vector1)
max(vector1)
which.max(vector1) #an welcher stelle=> gibt den Index aus
subset(vector1,vector1>10)

subset(vector1,vector1>5)
mean(subset(vector1,vector1>5))
subset(vector1,vector1==0 | vector1==1.1) #woe wende ich which bei subsets an
which(vector1==0 |vector1==1.1 )
which(vector1 %in% c(0,1.1))



#Aufagbe4
length<-c(2.5,3.4,4.8,3.1,1.7)
diameter<-c(0.7,0.4,0.5,0.5,0.9)
volumen<-length*diameter*diameter*pi
volumen<-volumen*1000

#Aufgabe5
x<-c(1,2,3,4,5)
y<-c(3,5,7,9)
union(x,y)
setdiff(x,y)
setdiff(y,x)
intersect(x,y)

#Aufgabe6

#m2<-matrix(c(sample(0:100,70, replace = TRUE),seq(0,18,by=2)),nrow=8,ncol=10,byrow=TRUE)
m1<-matrix(c(seq(0,18,by=2),sample(0:100,70, replace = TRUE)),nrow=8,ncol=10,byrow=TRUE)
rowMeans(m1)
sd(rowMeans(m1))
#m2<-matrix(c(m1[2,],m1[3,],m1[4,],m1[5,],m1[6,],m1[7,],m1[8,]),nrow=7,ncol=10,byrow=TRUE)
m2<-matrix(c(m1[-1,]),nrow=7,ncol=10,byrow=TRUE)
colMeans(m2)
hist(colMeans(m2))


#Aufgabe6

#install.packages("tidyverse")
library(ggplot2)
library(tidyverse)
help(mpg)
names(mpg)
head(mpg)



str_mpg<-tibble(name=character(), type=character(),level=character(),dc=character())
str_mpg %>% add_column(name=c(names(mpg)))

str_mpg %>% add_column(x=c("dis","Con","con","dis","con","con","con","dis","dis","dis","con","con"))

tibble(names(mpg))
tibble(x=c("dis","Con","con","dis","con","con","con","dis","dis","dis","con","con"))

