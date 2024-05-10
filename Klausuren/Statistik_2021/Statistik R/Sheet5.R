
#Aufgabe1
x<-c(2,6,3,4,5)
y<-c(3,7,4,7,6)

dat<-tibble(x,y)

dat%>% select(x)


plot(x=dat$x,y=dat$y)

cov(x,y) #Einheitsabhängig

#coefficient of correlation Sxy/Sx *Sy sx und sy ist nicht mean
#Standardabweichung => sy und dx

#cov(x,y)/(sd(x)*sd(y)) 
cor(x,y) #Wert liegt immer zwischen -1 und 1
#regression line: criterion variable Y and predictor variable X
abline(lm(dat$y ~ dat$x),col="red") #x unabhängig y abhängig
abline(lm(dat$x ~ dat$y),col="blue")

#Aufgabe2

xi<-c(10,9,9,11,10,10,6,10,8,12,9,4,12)
yi<-c(5,5,4,6,7,5,3,4,5,7,4,2,8)
dat2<-tibble(xi,yi)

#a)
plot(dat2$xi,dat2$yi, xlim = c (-1,13) ,ylim=c(-1,10) )

#b)
#positive association:one variable (Y) increases with the second variable (X)
#negative association: Y decreases as X increases

#there is a positive asscocation

#c)
cov(xi,yi)
cor(xi,yi)

#d)




#e) ablesen von der Regression line 8
#Rechnersiche lösen
regl<-lm(dat2$yi ~ dat2$xi)
a<-regl$coefficient[1]
b<- regl$coefficient[2]

#y ausrechenen für x=8
a+8*b

segments(8,a+8*b,0,a+8*b)
segments(8,0,8,a+8*b)

c(3)->y

#f) ?

#g)

cor(xi,yi)*cor(xi,yi)
#h)
dat2<-dat2 %>% add_row(xi=20,yi=0)
plot(dat2)
abline(lm(dat2$yi ~ dat2$xi),col="blue")

#Discriptive Statistics
#Aufgabe1

x<-c(3,8,7,9,5)
y<-c(6,7,10,8,4)

dat3<-tibble(x,y)

#a)
#Spareman's Rank
rank(x)
cor(rank(x,na.last=FALSE),rank(y,na.last=FALSE))


#Aufgabe2
library(tidyverse)
tab <- as_tibble(matrix(c(40,10,20,10,10,10),nrow=3,ncol=2,byrow=TRUE))
tab
chisq.test(tab)$expected


#Aufgabe3
class<-c(1,2,3,4)
pass<-c(325,285,706,885)  
 chisq.test(tab2)
 

 
