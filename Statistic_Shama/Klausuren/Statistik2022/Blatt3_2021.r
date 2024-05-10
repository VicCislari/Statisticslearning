library(tidyverse)
library(ggplot2)
library(psych)
tidyr::who
######b
b<-  who %>%  gather(new_sp_m014:newrel_f65, key = "key", value = "cases",
                 na.rm = TRUE)
b
### c

c<- b %>%  count(key)
c

