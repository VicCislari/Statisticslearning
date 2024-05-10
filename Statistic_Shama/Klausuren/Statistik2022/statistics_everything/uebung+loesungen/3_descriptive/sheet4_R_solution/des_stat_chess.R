#####################################################
# Descriptive Statistics: Chess Players
#
# File: des_stat_chess.R
#
###################################################### 
# An experiment compared the ability of three groups of
# participants to remember briefly-presented chess 
# positions. The data are shown below. The numbers 
# represent the number of pieces correctly remembered 
# from three chess positions. 
# Compare the performance for each group by computing 
# mean, median, min, max, quartils, interquartil range, 
# variance. Create side-by-side box plots for these 
# three groups. What can you say about the differences
# between these groups from the box plots?

#########################################################
# old fashion solution
#########################################################
data <- matrix(c(
  22.1,32.5,40.1,
  22.3,37.1,45.6,
  26.2,39.1,51.2,
  29.6,40.5,56.4,
  31.7,45.5,58.1,
  33.5,51.3,71.1,
  38.9,52.6,74.9,
  39.7,55.7,75.9,
  43.2,55.9,80.3,
  43.2,57.7,85.3), nrow=10, ncol=3, byrow=TRUE)
colnames(data) <- c("Non-players","Beginners","Tournament")

char_numbers <- rbind(
  apply(data,2,min),
  apply(data,2,max),
  c(quantile(data[,1],probs=c(0.25),type=1),
    quantile(data[,2],probs=c(0.25),type=1),
    quantile(data[,3],probs=c(0.25),type=1)),
  c(quantile(data[,1],probs=c(0.5),type=1),
    quantile(data[,2],probs=c(0.5),type=1),
    quantile(data[,3],probs=c(0.5),type=1)),
  c(quantile(data[,1],probs=c(0.75),type=1),
    quantile(data[,2],probs=c(0.75),type=1),
    quantile(data[,3],probs=c(0.75),type=1)),
  apply(data,2,mean),
  apply(data,2,var))
char_numbers <- rbind(char_numbers, char_numbers[5,] - char_numbers[3,])
rownames(char_numbers) <-c("min","max","q1","q2","q3","mean","variance",
                           "interquartil range")
char_numbers

# Boxplots
boxplot(data[,1],data[,2],data[,3], names=colnames(data),
        main = "side by side boxplots",
        xlab = "player type", ylab = "rem. chess positions")

#########################################################
# Solution with tibbles
#########################################################
library(tidyverse)
data <- tibble(
    type = c(rep("non-player",10), rep("beginner",10),rep("tournament",10)),
    res = c(22.1,22.3,26.2,29.6,31.7,33.5,38.9,39.7,43.2,43.2,
            32.5,37.1,39.1,40.5,45.5,51.3,52.6,55.7,55.9,57.7,
            40.1,45.6,51.2,56.4,58.1,71.1,74.9,75.9,80.3,85.3))
measures <- data %>%
  group_by(type) %>%
  summarise(Min = min(res),Mx=max(res),
            q1=quantile(res,0.25,type=1),q2=quantile(res,0.5,type=1),
            q3=quantile(res,0.75,type=1),
            Mean=mean(res),variance=var(res),
            interquartile_range=q3-q1)

measures

# Boxplots
boxplot(data$res~data$type, data, 
        main = "side by side boxplots",
        xlab = "player type", ylab = "rem. chess positions")

# eps-file
dev.copy2eps(file="../pictures/chess_bp.eps")
