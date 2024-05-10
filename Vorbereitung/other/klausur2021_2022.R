library(tidyverse)
library (ggplot2)
library(DescTools)
library(lattice)
library(dplyr)
Melanoma
    ###################### 1A ################# 
s <- Melanoma
 view(s);head(s)
  
 ###################### 1b ################# 
 b<- tibble(name = character(), type = character (),
                 level = character())
                 
 b %>%  add_row (name = "time", type = "ordinal", level = "Quantitative") %>% 
        add_row (name = "status", type = "Nominal/ordinal", level = "Quantitative") %>%
        add_row (name = "sex", type = "Nominal", level = "Quantitative") %>%
        add_row (name = "age", type = "interval", level = "Quantitative") %>%
        add_row (name = "year", type = "interval", level = "Quantitative") %>%
        add_row (name = "thickness", type = "ratio", level = "Qualitative") %>%
        add_row (name = "ulcer", type = "absolute", level = "Quantitative")
      
                 #######################  1 C ###########
 
  c<-s %>%  toString( "sex", "status", "ulcer")
 c
   c<-s%>% mutate(live.status = case_when(s$status == 1 ~ "died",
                                          s$status == 2 ~ "alive")) %>% 
           dplyr::select(sex, live.status)
         
c   
   #######################  1 D ###########
   
   
    d <- table(c$live.status,c$sex ) %>% addmargins()
    d 
      chisq.test(d)$statistic
      chisq.test(d)$expected
      
  #######################  1 F ###########
      
   f <- s %>% group_by(s$sex) %>% summarise (min = min(age),
                                            max = max(age),
                                            mean = mean(age),
                                            q1 = quantile(age, 0.25, type = 1),
                                             q2 = quantile(age, 0.5, type = 1),
                                             q3 = quantile(age, 0.75, type = 1))
      f
      
      #######################  1 G ###########
      
      
      boxplot(s$age~s$sex, main = "Side Boxplot", ylab = "AGE", xlab = "SEX", 
              col = "darkred")
      
      
      #######################  1 H  ###########
      
      
      
      
      
   
   
   
   
   
   
   













library(dplyr)
library(mlbench)

data(BostonHousing)
head(BostonHousing)
# Auswahl von zwei Spalten
Boston2 <- BostonHousing %>% select(age, nox)
# Auswahl von allen Spalten zwischen "nox" und "age"
Boston2 <- BostonHousing %>% select(nox:age)
# Auswahl der ersten 5 Spalten
Boston2 <- BostonHousing %>% select(1:5)








