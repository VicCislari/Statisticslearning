library (tidyverse)

cafe <- tibble (cafe=c(1,2,3,4,5),
                x=c(3,8,7,9,5),
                y=c(6,7,10,8,4)
  
)
cafe 

cafe %>% mutate(R_x=rank(x), R_y =rank (y))


cor(x=cafe$x,y=cafe$y,method = "spearman" )
########################################

#2a
results <-tibble(Attendence = c("Over 70%", "30%-70%", "Under 30%","Totals"),
                 Pass = c(40,20,10,70),
                 Fail = c(10,10,10,30),
                 Totals = c(50,30,20,100)
)                 

result <-tibble(Attendence = c(rep("Over 70%",50),rep("30%-70%",30),
                               rep("Under 30%",20)),
                course_result = c(rep("pass",40), rep("fail",10),
                                  rep("pass",20),rep("fail",10),
                                  rep("pass",10),rep("fail",10))
               )
#3b X²

      chi_test <-  result%>%table() %>% chisq.test()
      chi <- chi_test$statistic
#c
      c <- (chi/(chi+length(result$Attendence)))^0.50
      length(result)
      
  #corr
      e <- min(length(result),
               length(result$Attendence))
              



      