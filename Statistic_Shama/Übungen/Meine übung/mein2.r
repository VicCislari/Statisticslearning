library(tidyverse)
#############Blatt 10 aufgbabe 9

m <- matrix (c(12,5,5,5,10,2,4,4,7), nrow = 3 , ncol= 3,byrow = TRUE,
             dimnames = list(c("0-9","10-9","20+"), 
                             c("Infield", "Outfield","Pitcher")))
m%>% addmargins()

r <- tibble(m)


          
res <- chisq.test(x=m )
res
res$observed %>% addmargins()
res$expected %>% addmargins()
res$statistic
res$statistic > qchisq(0.95, df = 4)


#####################aufgabe 11
ibs<- 16.
online <- tibble(no_auction= c( 18.19, 16.98, 19.97, 16.98, 18.19, 15.99, 13.79,
                                15.90, 15.90, 15.90,15.90, 15.90, 19.97, 17.72),
                auction =c(10.50, 12.00, 9.54, 10.55, 11.99, 9.30, 10.59, 10.50,
                           10.01, 11.89,11.03, 9.52, 15.49, 11.02))

 online %>% summarise(mean.no = mean(no_auction, na.rm = TRUE),mean.au = mean(auction, na.rm = TRUE),
                          variace.no = var(no_auction, na.rm = TRUE),variace.au = var(auction, na.rm = TRUE),
                          sd.no = sd (no_auction, na.rm = TRUE),  sd.no = sd(auction, na.rm = TRUE), 
                          var.co.no = sd.no/mean.no ,var.co.auc = sd.au/mean.au   )

                                
     cd<- sd(online$auction)/mean(online$auction)*100                    
                                
                                
     online%>% summarise(
       mean.noa = mean(noa, na.rm = TRUE), mean.auc = mean(auc, na.rm = TRUE),
       var.noa = var(noa, na.rm = TRUE), var.auc = var(auc, na.rm = TRUE),
       sd.noa = sd(noa, na.rm = TRUE), sd.auc = sd(auc, na.rm = TRUE),
       var.coeff.noa = sd. mean.noa, var.coeff.auc = sd.auc/mean.auc)        
                        
                        
                        
                      )
                          
















