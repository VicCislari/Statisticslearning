library(tidyverse)
library(ggplot2)
x <- c(2,6,3,4,5)
y <- c(3,7,4,7,6)

plot (x,y, main = "Scatterplot", type = "l") 
##1a
cov(x,y)
mean(x);var(x)
mean(y);var(y)
##1b
cor(x,y)   
#1c  
a <- lm (y~x)  #regression line = y =b*x+a 
a
summary(a)
s<- lm (x~y)
s
summary(s)
boxplot(x,y)
boxplot(y)

alpha <- lm(x~y)$coefficients[1]
beta <- lm(x~y)$coefficients[2]
alpha
a_strich <- -alpha/beta
b_strich <- 1/beta
summary (a_strich)
a_strich
b_strich


plot(x,y, main = "Scatterlot", sub = "regression y ~ x  (blue), x ~ y (red) ")

abline(lm (y~x), col="blue")
abline(a=a_strich,b=b_strich, col= "red")




########################### Aufgbae 2
class <- tibble (x = c(10,9,9,11,10,10,6,10,8,12,9,4,12),
                 y = c(5,5,4,6,7,5,3,4,5,7,4,2,8)
)
head(class)
boxplot(class$y)
# 2a
plot(class$x,class$y, main ="Scatterplot", type = "l")
# 2c
mean(class$x)
mean (class$y)

cor(class$x, class$y)
cov (class$x, class$y)

#2d  y = a+b*X

f <-  lm(class$y~class$x)
f
aalpha <- lm(class$y ~class$x)$ coefficients [1] 
bbeta <- lm(class$y ~ class$x)$ coefficients [2] 

 

aa_strich <- -aalpha/bbeta
aa_strich
bb_strich <- 1/bbeta
bb_strich

plot(class$x,class$y, main = "Scatterlot", xlab = "time spent", ylab = "score",  xlim = c(-1,13),
     ylim = c(-1,10),)
abline(lm (class$y~class$x), col="blue")

### 2e
test_score <- a+b*8;test_score

## 2f
abline (lm (class$y ~class$x), col = "Red")


########DESCRIPTES STATISTICS 1a
c <- tibble (cafe =c (1:5), x = c(3,8,7,9,5),y=c (6,7,10,8,4));c
#1a
cor (x= c$x, y=c$y, method = "spearman")
s<-c %>% mutate(R_x = rank(x),
             R_y = rank (y))
s
#1b
w <- s %>% mutate(R_x1 = rank (-x),
             R_y2 = rank (-y))
w

#1c

c_<-tibble(quality = c("good", "bad"), x =c(2,3),y=c(1,4));c_

odd_ratio <- ((2/5)/(3/5))/((1/5)/(4/5));odd_ratio

###############################Aufgabe 2
library(DescTools)
contigency <- tibble(attendence = c(rep("Over 70%", 50),rep( "30% - 70%", 30), rep ("under 30%",20)), 
                     result =c(rep("pass",40), rep ( "fail",10),
                               rep("pass",20), rep ( "fail",10),
                               rep("pass",10), rep ( "fail",10)));contigency
                               
table <- table(contigency)
table
                               
xy <- chisq.test(table)
xy
# contingency table
xy$observed
# indifference table
xy$expected
# Chi^2
xy$statistic


ContCoef(table) #'ODER'
Assocs(table)

ContCoef (table)
# install.packages("vcd")
library(vcd)

assocstats(table)
Assocs(table)
Phi(table) # 0.4714045
CramerV(table) # 0.4714045  
Phi(table)
ContCoef(table)
CramerV(table)
TschuprowT(table)
###################### Aufgbe 3a

# 
raw_data <- tibble( Travel_class= c(rep("First class", 325),rep("Second class", 285),rep("Third class", 325),rep("Staff class", 885)) ,
                   Rescued_status = c(rep("First class", 122),rep("Second class", 167),rep("Third class", 528),rep("Staff class", 673)) ,
                   Not_Rescued_status = c(rep("First class", 203),rep("Second class", 118),rep("Third class", 203),rep("Staff class", 212) )

 )

library(titanic)
ti<-as.tibble(Titanic)
ti
raw_data <- tibble(First_class = c(rep ("Not_Rescued", 122),rep ("Rescued", 203)),
                   Second_class = c(rep ("Not_Rescued", 167),rep ("Rescued", 118)),
                   Third_class = c(rep ("Not_Rescued",528),rep ("Rescued", 178)),
                   Staff       =  c(rep ("Not_Rescued", 673),rep ("Rescued", 212))
                   )  
  
raw_data
view(raw_data)
head(raw_data)






































