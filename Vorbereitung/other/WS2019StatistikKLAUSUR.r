library(tidyverse)

raw_data<-MASS::Cars93
raw_data %>% select(Type)

#Aufgabe1 seite 3

#b
mean(raw_data$Price)  #19.51
quantile(raw_data$Price,0.25) #12.2
quantile(raw_data$Price,0.75) #23.3

boxplot(raw_data$Price)


#c



t1<- raw_data %>% select ( Type,Origin)
t1

tab<-table(t1)
chisq.test(t1$Type,t1$Origin)$expected # indifference
chisq.test(t1$Type,t1$Origin)$statistic  #corresponsding


#Aufgabe2
#i)
#without replacement
choose(60,5)*choose(40,5)/choose(100,10)
dhyper(5,40,60,10)

#with
#P(X=5)
dbinom(5,10,0.4)

#ii)

#m
#P(10m)=dhyper(10,60,40,10)

(16/60)^2*(4/6)^3

0.6^10*(4/6)^3*(16/60)^2*(4/60)^5*(factorial(10)/(factorial(3)*factorial(2)*factorial(5)))/0.6

((40^3*16^2*4^5)/60^10)*(factorial(10)/(factorial(3)*factorial(2)*factorial(5)))/0.6^10

P(X|M)=P(XandM)/P(M)=?0.0115
#iii)
#nonEU 10/100
#(nonEU)'=90/100

#with replacement
(90/100)^9*(10/100)
#without replacement
(choose(90,9)/choose(100,9))*(choose(10,1)/choose(100,1)) # 0.03712757
dbinom(1,10,100/100)

#Aufgabe3

#a) E(x)=1*0.3+ 2*0.3+ 3*0.2+ 4*0.1+ 5*0.1=2.4 100*E(x)=100*2,4=240
  #E(x^2)-E(X)^2=(1^2*0.3+ 2^2*0.3+ 3^2*0.2+ 4^2*0.1+ 5^2*0.1 - 2.4^2)*100=164
  #sd=164^0.5=12.81

#b)N(240,12.81)
#P(X<=250)=phi(250-240)
pnorm(250,240,12.81)

#c)
qnorm(0.99,240,12.81) #270 tickets

x<-c(250:280)
cbind(x,pnorm(c(250:275),240,12.81))

#d)
y<-c(70:100)
cbind(y,pnorm(250,2.4*y,(1.64*y)^0.5)) #92





#Aufgabe4
library(TeachingDemos)

x4<-c(1.013,0.921,0.971,1.064,0.887,0.954,1.015,0.953,1.068,0.880,0.760,0.863,0.994,1.007,0.769,0.941,1.052,0.998,0.862,0.917)

#a)
t.test(x4,alternative = "two.sided",conf.level = 0.95)  #[0.9031564 ,0.9857436]

#lower
mean(x4)-qt(1-(0.95/2),df=length(x4)-1)*sd(x4)*length(x4)^0.5
#upper
mean(x4)+qt(1-(0.95/2),df=length(x4)-1)*sd(x4)*length(x4)^0.5

#b)
sigma.test(x4,alternative = "less",conf.level = 0.99)

n<-length(x4)
(((n-1)*sd(x4)^2)/(qchisq(1-0.99,df=n-1)))^0.5 #[0.1392067,inf]

#c)
mean(x4)+qt(1-0.90,df=length(x4)-1)*sd(x4)/(length(x4))^0.5

#Aufgabe5
#a) binomal distribution

#b) Ho: p =0.25 H1: p!=0.25

#c) R=(-inf,-0.06270678)u(0.06270678,inf)
qnorm(1-(0.95/2))

x<-c(0:20)

t<-((x/20)-0.25)/((0.25*0.75/20)^0.5)

cbind(x,(2*(1-pnorm(((x/20)-0.25)/((0.25*0.75/20)^0.5))))-0.025)

((9/20)-0.25)/((0.25*0.75/20)^0.5)
qnorm()

#d)
binom.test(8,20,0.25,alternative = "two.sided",conf.level = 0.95) #p value 0.1261 p>alpha => dont reject Ho


#e)
pnorm(0.25+qnorm(1-0.95)*sqrt(0.25*0.75/20),0.25,sqrt(0.25*0.75/20))
