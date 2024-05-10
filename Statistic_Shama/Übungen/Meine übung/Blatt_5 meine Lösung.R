library(tidyverse)
library(ggplot2)
library(psych)
############ Augabe 1a)
x <- c(2,6,3,4,5)
y <- c(3,7,4,7,6)
#t <- tibble(x,y)
x;y


 cov(x,y)
 cor(x,y)
 lm (x~y)
 a <- lm(x~y) $ coefficients[1]
 b <- lm(x~y) $ coefficients[2]
 
 lm(y~x)
 alp <- lm(x~y) $ coefficients[1]
 bet <- lm(x~y) $ coefficients[2]
 
 
 astrich <- -alp/bet
 bstrich <- 1/bet
 astrich
 bstrich
 par (mfrow = c(1,1))#
 plot(x,y, main = "scatterplot", xlim = c(0,7), ylim =c(0,8),lwd = 1,ylab = c("DF"), type = "b"#mit Type L kommt eine linie
      ,      sub = "regression : y~x (blau), x~y (rot)")
      
 abline (lm(y~x),col = "red")
 abline (a= astrich,b=bstrich,col="blue")
 
 
 #### Aufgabe 2
 
 suli <- tibble( xi = c(10,9,9,11,10,10,6,10,8,12,9,4,12),
                  yi = c(5,5,4,6,7,5,3,4,5,7,4,2,8)
 )
 suli
 #### Aufgabe 2a
 par(mfrow =c(1,1))
 plot(class$xi,class$yi,main = "time spent in exercise and sco", xlim = c(0,20),
      ylim = c(0,8), lwd =2, xlab = "time spent" , ylab = "score", col= c("red"))
 
 #### Aufgabe 2c
 cov(class)
 cov(class$xi,class$yi)
 cor(class)
 cor(class$xi,class$yi)
 
 lm(class$xi~class$yi)
 
 a <- lm(class$xi~class$yi)$ coefficients [1]
 b <- lm(class$xi~class$yi)$ coefficients [2]
 
###################### Aufgabe 3a
 rated <- tibble(cafe = c(1,2,3,4,5),
                   x  = c(3,8,7,9,5),
                   y  = c(6,7,10,8,4))
 rated
 
 # R = 0.6
 cor.test(rated$x, rated$y, method = "spearman")
 
 rated <- rated %>% mutate(R_x = rank (x),
                           R_y = rank (y))
      
 ######Aufgabe 3b
rated <- rated %>% mutate( R_desc_x = rank(-x),
                           R_desc_y = rank(-y))

rated
 
#############Aufgbae 3c
rate2 <- tibble( quality = c(1,2),
                 x       = c(2,3),
                 y       = c(1,4)
)
rate2

OR <- ((2/5)/(3/5))/((1/5)/(4/5))
OR



  
 
 
 
 
 
 
 
 
 
 
 
 
 
 

  
 
 
 
 
 
 
 
 
 
 
 
 
 
 