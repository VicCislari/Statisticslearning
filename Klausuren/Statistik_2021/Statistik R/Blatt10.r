library(tidyverse)

x <??? c (7.06, 11.84, 9.28, 7.92, 13.5, 3.98, 3.82, 7.34, 8.7, 9.24, 4.86, 3.32,
        12.78, 12, 5.24, 11.4, 6.56, 9.04, 7.72, 9.26, 7.88, 8.6, 9.3, 8.42, 8.54)

y <??? c (8.68, 6, 6.3, 10.24, 10.88, 5.36, 7.82, 4.7, 9.02, 9.78, 6.9, 5.8, 13.56,
        10.32, 13.3, 11.38, 7.94, 10.74, 13.68, 14.92, 7.42, 10.36, 10.54,
        5.22, 13.74, 12.98, 10.34, 10.02, 17.8, 13.04, 5.2, 9.4, 11.18, 12.68,
        12.36)

alpha <- 0.05


t.test(x,y,alternative = "less", conf.level = 1-alpha)
##################### aufgabe 3 
w<-c(16,15,11,20,19,14,13,15,14,16)
al<-c(13,13,10,18,17,11,10,15,11,16)

alphaa <- 0.05

t.test(w,al,alternative = "less", conf.level = 1-alpha)


t.test(w,al,alternative = "two.sided", paired = TRUE, conf.level = 1-alpha)


###########################Aufgabe 4

A<-c(102.4,101.3,97.6,98.2,102.3,99.1,97.8,103.9,101.6,100.1)
B<-c(98.4,101.7,100.5,99.3,100.6,99.6,102.2,101.1,99.9,101)

alphaaa <- 0.05

var.test(A,B,alternative = "greater", paired = FALSE, conf.level = 0.95)

##########################aufgabe 5
catA<-c(7.2,4.1,5.5,4.5,5.7,3.8,4.6,6,5.2,5.4)
catB<-c(5.3,4.4,5,3.5,3.9,4.9,5.6,2.5,4,3.6)


mean ( catA ) ; var ( catB )
mean ( catB ) ; var ( catA )


alppha <- 0.1


var.test(catA,catB,alternative = "two.sided", paired = FALSE, conf.level = 1-alppha)
###############5b
alpppha <- 0.05

t.test(catA,catB,alternative = "less", paired = FALSE, var.equal=TRUE,conf.level = 1-alpppha)

#####################aufgabe 6a

n <??? 230 ; def <??? 31; p0 <- 0.1; 
binom.test(x = def, n = n, p = p0, 
           alternative = "greater", conf.level = 1-alpha)

fisher.test(cont.tab, alternative = "less", conf.level = 1-alpha)
binom.test(32,230,0.1,"greater",conf.level = 1-alpha)

####################################### 6b

n1 <- 230; def1 <- 31
n2 <- 116; def2 <- 7
tib <- tibble(c(n1, def1),c(n2, def2))

fisher.test(tib, alternative = "less", conf.level = 1-alpha)

############################################## 7


