library(tidyverse)
#Aufgabe1

#a)
#omega={(1,1),(1,2),...(1,6),(2,1)..(2,6)..}
#     ={(i,j)|i,j???{1,2,3,4,5,6}}
#|omega|=6²=36

#b)
#A={(1,1),(1,2),(1,3),(1,4),(1,5),(1,6)}
#B={(1,6),(6,1),(2,5),(5,2),(3,4),(4,3)}
#AuB={(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(6,1),(2,5),(5,2),(3,4),(4,3)}
#A^B={(1,6)}
#A'^B'=(AorB)'= omega/(AorB)

#Aufgabe2
#a)P(A\B)=P(A)-P(AandB)=1/3-1/10  //The probability that eventA occurs without event B
#b)P(AorB)=P(A)+P(B)-P(AandB)=   //The probability that event A or event B occurs
#c)P(AandB)'=1-P(AandB)
#d)P(AorB)'=1-P(AorB)
#e)(A'andB)'=1-(B-(AandB))=1-P(B)+P(BandA)=17/20

#Aufgabe3
#a)P(AorBorC)=P(A)+P(B)+P(C)-P(AandB)-P(AandC)-P(BandC)+P(AandBandC)
#           =0.3+0.2+0.4-0.04-0.1-0.1+0.01=0.67
   
#b)P(AorBorC)'=1-P(AorBorC)=1-0.67=0.33
#c)P(AandB'andC')orP(A'andBandC')orP(A'andB'andC)=(P(A)*(1-P(B))*(1-P(C))) + ((1-P(A))*P(B)*(1-P(C))) +((1-P(A))*(1-P(B))*P(C))= 0.452
#d)P(AandBandC')orP(AandB'andC)orP(A'andBandC)=(P(A)*P(B)*(1-P(C))) + (P(A)*(1-P(B))*P(C)) +((1-P(A))*P(B)*P(C))= 0.452
#d)Lösung P ((A ??? B) ??? (A ??? C) ??? (B ??? C) \ (A ??? B ??? C))

#Aufgabe4

#a)
CoinPlot<-function(n){
  numb_tosses<-tibble(
    number_of_tosses=1:n,
    coin=sample(x=c(0,1),size=n,replace=TRUE),    #(0,1) 0->head 1->tail sample gibt n-mal 0 oder 1 aus, replace-> null und eins mehrfach 
    cumulative_sum=cumsum(coin)/number_of_tosses
    )

  print(numb_tosses)

  plot(numb_tosses$number_of_tosses,numb_tosses$cumulative_sum)
  
  #theme_classic()
}

CoinPlot(10)
CoinPlot(100)
CoinPlot(1000)
CoinPlot(10000)


########### ggplot statt plot
coinPlot2 <- function(n) {
  # create a tibble containing the result of n tosses
  tosses <- tibble(
    # number of flip
    no = 1:n,
    # result 
    coin = sample(x=c(0,1), size = n, replace = TRUE),
    # average
    avg.no = cumsum(coin)/no)
  # create a diagram
  ggplot(data = tosses) +
    # scatterplot of the points (no, avg.no)
    geom_point(mapping = aes(x=no,y=avg.no)) +
    # connecting the points
    #    geom_line(mapping = aes(x=no,y=avg.no)) +
    # horizontal line: probability of a head
    geom_hline(yintercept = 0.5) +
    # setting the bounds for y
    ylim(0,1) +
    # title and labels of the axis
    ggtitle(paste(n," tosses of a fair coin"), 
            subtitle = "average number of head") +
    xlab("n") +
    ylab("mean") +
    # theme
    theme_classic()
}

#b)
coinPlot(10)
coinPlot2(100)
coinPlot2(1000)
coinPlot2(10000)

#Aufgabe5
library(gtools)
b1<-c("a","b","c")
b2<-c("a","a","b","c")
balls<- c(rep("red",5),rep("blue",3),rep("green",2))

#a)
combinations(length(b1),2,b1,repeats.allowed = TRUE) #mit zurücklegen with replacement
combinations(length(b1),2,b1,repeats.allowed = FALSE) #Ziehen ohne zurückkegen without replacement

permutations(length(b1),2,b1,repeats.allowed = TRUE)  #Reihenfolge beachtet mit zurücklegen
permutations(length(b1),2,b1,repeats.allowed = FALSE) #Reihenfolge beachtet ohne zuücklegen

combinations(length(b2),2,b2,set=FALSE,repeats.allowed = TRUE)
combinations(length(b2),2,b2,set=FALSE,repeats.allowed = FALSE)

#b)
sample(c("red","blue","green"),size=10,replace=TRUE,prob = c(0.5,0.3,0.2))
sample(c(rep("red",5),rep("blue",3),rep("green",2)),size=10,replace=TRUE)

sample(c(rep("red",5),rep("blue",3),rep("green",2)),size=10,replace=FALSE) #ohne zurücklegen




#c)  & #d)

#omega= the combinations where repeats arent allowed

#for the Urne model

#I Drawing with replacement with respect to the order
omega1<-permutations(3,3,balls,set=TRUE,repeats.allowed = TRUE) # |omega|=27

first<- tibble(V1=omega1[,1],V2=omega1[,2],V3=omega1[,3],
               probability=(case_when(V1=="red" ~ 0.5,
                                      V1=="blue" ~ 0.3,
                                      V1=="green" ~ 0.2)*
                              (case_when(V2=="red" ~ 0.5,
                                         V2=="blue" ~ 0.3,
                                         V2=="green" ~ 0.2))*
                              (case_when(V3=="red" ~ 0.5,
                                         V3=="blue" ~ 0.3,
                                         V3=="green" ~ 0.2))))


#FALSCH
#II Drawing with replacement without observing the order
omega2<-combinations(3,3,balls,set=TRUE,repeats.allowed = TRUE)
second<- tibble(V1=omega2[,1],V2=omega2[,2],V3=omega2[,3],
               probability=(case_when(V1=="red" ~ 0.5,
                                      V1=="blue" ~ 0.3,
                                      V1=="green" ~ 0.2)*
                              (case_when(V2=="red" ~ 0.5,
                                         V2=="blue" ~ 0.3,
                                         V2=="green" ~ 0.2))*
                              (case_when(V3=="red" ~ 0.5,
                                         V3=="blue" ~ 0.3,
                                         V3=="green" ~ 0.2))))

#III Drawing without replacement with respect to the order
omega3<-permutations(10,3,balls,set=FALSE,repeats.allowed = FALSE)

#So wie in den Lösungen

#Seite 72 im SKript
third2<- tibble(V1=omega3[,1],V2=omega3[,2],V3=omega3[,3],
               numb_red=(if_else(V1=="red",1,0)+if_else(V2=="red",1,0)+if_else(V3=="red",1,0)),
               numb_blue=(if_else(V1=="blue",1,0)+if_else(V2=="blue",1,0)+if_else(V3=="blue",1,0)),
               numb_green=(if_else(V1=="green",1,0)+if_else(V2=="green",1,0)+if_else(V3=="green",1,0)),
               prop=((choose(5,numb_red)*choose(3,numb_blue)*choose(2,numb_green))/choose(10,3))
                 ) %>% unique()





#IV Drawing without replacement without observing the order
omega4<-combinations(10,3,balls,set=FALSE,repeats.allowed = FALSE)
four<- tibble(V1=omega4[,1],V2=omega4[,2],V3=omega4[,3],
                numb_red=(if_else(V1=="red",1,0)+if_else(V2=="red",1,0)+if_else(V3=="red",1,0)),
                numb_blue=(if_else(V1=="blue",1,0)+if_else(V2=="blue",1,0)+if_else(V3=="blue",1,0)),
                numb_green=(if_else(V1=="green",1,0)+if_else(V2=="green",1,0)+if_else(V3=="green",1,0)),
                prop=((choose(5,numb_red)*choose(3,numb_blue)*choose(2,numb_green))/choose(10,3))
) %>% unique() %>% select(V1,V2,V3,prop)


#########Independence and Conditional Probabilities#############################
#Aufgabe1

#Folie43

#P(A|B)=P(AandB)/P(B)=(1/10)/(1/4)
#P(B|A)=


