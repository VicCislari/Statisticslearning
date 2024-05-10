library (tidyverse)
library(ggplot2)
library(psych)
   party = c("CDU", "SPD","AfD","FDP","DIE LINKE","GRUENE","CSU","Others")
   results2013 = c(26.8,20.5,12.6,10.7,9.2,8.9,6.2,5.0 )
  results2017 = c(34.1, 25.7, 4.7, 4.8, 8.6, 8.4, 7.4, 6.2)
  lbs <- paste(party)
  
  pie (results2017, labels = paste (party,"(",results2017,"%)"), main = "R.2017")  
  
    
   pie (results2013, main = c("r.2013") ,
        labels=paste(party,"(",results2017,"%)")  )  
  
  election<-table(party,result$results2013,result$results2017)
  
  barplot(results2013,names.arg=party, main = c("Result from 2013"))
  
  barplot(results2017,names.arg=party, main = c("Result from 2017"))
  
  #########AUFGABE 2###############
  
  times<-c(568, 581, 640, 641, 645, 657, 673, 696, 703, 720, 728, 729, 777, 808,
           824, 82565, 8, 875, 1007)
  
  abs <- table(times)
  
  rel <- round (prop.table(table(times)*100),2)
  cum <- round(cumsum(prop.table(table(times))*100),2)
  table <- cbind(abs, rel, cum)
 table
 
 ############Aufgabe 5 ################
 
 num <- tibble( nr = c(1,2,3,4,5,6,7,8),
                ab_fre = c(5,4,1,7,2,3,1,2)
    )
 num
 x <- rep(num$nr, num$ab_fre)
 x
 summe <- sum(num$ab_fre)
 summe
 mean(x)
 geometric.mean(x)
 harmonic.mean(x)
 mean(x, trim = 0.1)
 
 ################ aufgabe 7
 

 x <- c(3,7,2,5,6,10,6,3,6,5)
 sort (x)

 v<-mean(x)
 quantile(x,c(0.25,0.5,0.75), type = 1)
 var(x)
 geometric.mean(x)
 harmonic.mean(x)
 mean (v, trim = 0.05/2)


 ##############AUFGABE 8 ##############
 
  p<- tibble(type= c (rep ("non_players ",10),rep ("beginners ",10),
                     rep ("tourn_pla",10)),
            res = c(22.1,22.3,26.2,29.6,31.7,33.5,38.9,39.7,43.2,43.2,
                    32.5,37.1 ,39.1 ,40.5 ,45.5,51.3,52.6,55.7,55.9 ,57.7,
                    40.1,45.6,51.2,56.4,58.1,71.1,74.9,75.9,80.3,85.3)
 ) 
 

 p%>%group_by(type)%>%summarise(mean = mean(res),
                                variance = var(res),
                                median = median(res),
                                min   = min(res),
                                max   = max(res),
                                q1    = quantile(res,0.25,type = 1) ,
                                q2    = quantile(res,0.5,type = 1),
                                q3    = quantile (res, 0.75, type = 1),
                                interquantile = q3-q1
                                                   )
 
 
 ###########Aufgabe 9 ###################

 

 d <- c(12.5,29.9,14.8,18.7,7.6,16.2,16.5,27.4,12.1,17.5)
 al <- c(342,1245,502,555,398,670,796,912,238,466)
 ############# a ############
  mean(d)
 mean(al)
median(d)
median(al)
 ########### b ##########
quantile(d, c(0.25,0.75), type = 1)
quantile(al,c(0.25,0.75), type = 1)
##########  c    #########

q1 <- quantile(d, c(0.25), type = 1)
q2 <- quantile(d, c(0.5), type = 1)
q3 <- quantile(d, c(0.75), type = 1)

quantile_d <- q3-q1 
quantile_d
sd (quantile_d)

q1 <- quantile(al, c(0.25), type = 1)
q2 <- quantile(al, c(0.5), type = 1)
q3 <- quantile(al, c(0.75), type = 1)

quantile_al <- q3-q1 
quantile_al

var(d)
var(al)
 
sd (d)
sd (al)
mean(d)

########################
library (ggplot2)
##### a 
?mpg()
### b
x<- cut(mpg$displ, breaks =c( 3,5,8 ))
tab2 <- mpg %>% select(displ, hwy) %>% 
  
mutate (displ_class = cut(displ,breaks=c(1,3,5,8),labels=c("low","medium","big")))
                 

tab2 %>% grou_by(displ_class)


# x<-cut(mpg$displ,breaks=c(3,5,8))
# tab2<-mpg%>% select(displ,hwy) %>% mutate(displ_class = cut(displ,breaks=c(1,3,5,8),labels=c("low","medium","big")))
# 
# tab2 %>% group_by(displ_class)
mean(hwy)
mean (displ)
min ()
max
quantile (d,c = 0.75, type = 1)






























 
  
  
  
  
  
  
       
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  