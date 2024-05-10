library(tidyverse)

shama <- tibble(sample (x=c(1:5), size = 10,replace = TRUE))
shama

library(gtools)
 B1 <- c("a","b","c")
 B2 <- c("a","a","b","c")
 
 balls <- c(rep("red",5), rep("blue",2),rep("green",2))
 #(rep("red",5),rep("blue",3),rep("green",2))
combinations(length(B1),2,B1,repeats.allowed = TRUE)
combinations(length(B1),2,B1,repeats.allowed = FALSE)
permutations(length(B1),2,B1,repeats.allowed = TRUE)
permutations(length(B1),2,B1,repeats.allowed = FALSE)

combinations(length(b2),2,b2,set=FALSE,repeats.allowed = TRUE)
combinations(length(b2),2,b2,set=FALSE,repeats.allowed = FALSE)


#b

sample (c("red","blau","grün"), size = 10,replace = TRUE,prob = c(5,3,2))
sample(c("red","blue","green"),size=10,replace=TRUE,prob = c(0.5,0.3,0.2))
sample(c(rep("red",5),rep("green",2),rep("blue",2)), size = 10, replace = TRUE)


Omega1<- permutations(3,3,balls, set = TRUE,repeats.allowed = FALSE)#3^3 = 27 |OMEGA|=27

First <- tibble(v1=Omega1[,1],v2=Omega1[,2],v3=Omega1[,3],
                probability = (case_when( v1=="red"~5,
                                          v1== "blue"~3,
                                            v1== "green"~2)*
  
                              (case_when(v2=="red"~5,
                                         v2== "blue"~3,
                                         v2== "green"~2))*
                  
                          (case_when(v3=="red"~5,
                                     v3== "blue"~3,
                                     v3== "green"~2)))
                               
                               
                )


#III Drawing without replacement with respect to the order
omega3<-permutations(5,3,balls,set=TRUE,repeats.allowed = TRUE)

#So wie in den Lösungen

#Seite 72 im SKript
third2<- tibble(V1=omega3[,1],V2=omega3[,2],V3=omega3[,3],
                numb_red=(if_else(V1=="red",1,0)+if_else(V2=="red",1,0)+if_else(V3=="red",1,0)),
                numb_blue=(if_else(V1=="blue",1,0)+if_else(V2=="blue",1,0)+if_else(V3=="blue",1,0)),
                numb_green=(if_else(V1=="green",1,0)+if_else(V2=="green",1,0)+if_else(V3=="green",1,0)),
                prop=((choose(5,numb_red)*choose(3,numb_blue)*choose(2,numb_green))/choose(10,3))
) %>% unique()



x <- tibble(sample (x=c(1:5), size = 10,replace = TRUE))



















