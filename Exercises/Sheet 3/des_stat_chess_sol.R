#####################################################
# Descriptive Statistics: Chess Players
# Solution
#
# File: des_stat_chess_sol.R
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

library(tidyverse)
data1 <- tibble(
    type = c(rep("non-player",10), rep("beginner",10),rep("tournament",10)),
    res = c(22.1,22.3,26.2,29.6,31.7,33.5,38.9,39.7,43.2,43.2,
            32.5,37.1,39.1,40.5,45.5,51.3,52.6,55.7,55.9,57.7,
            40.1,45.6,51.2,56.4,58.1,71.1,74.9,75.9,80.3,85.3))
data1

# alternative: tidy the messy dataset data
data %>% 
  as_tibble() %>%
  gather(key = "type", value = "res")

measures <- data1 %>%
  group_by(type) %>%
  summarise(Min = min(res),Max=max(res),
            q1=quantile(res,0.25,type=1),q2=quantile(res,0.5,type=1),
            q3=quantile(res,0.75,type=1),
            Mean=mean(res),variance=var(res),
            interquartile_range=q3-q1)
measures

# Boxplots
boxplot(data[,1],data[,2],data[,3], names=colnames(data),
        main = "side by side boxplots",
        xlab = "player type", ylab = "rem. chess positions")

# Boxplots with ggplot
boxplot(res ~ type, data = data1)
# solution with ggplot()
# changing the order in the side by side boxplots by adding a factor to type
data$type <- factor(data$type, levels = c("non-player", "beginner","tournament"))
ggplot(data = data1) + 
  geom_boxplot(mapping = aes(x=type, y=res, group = type)) +
  geom_point(mapping = aes(x=type,y=res,group=type)) +
  xlab("player type") +
  ylab("rem. chess positions") +
  ggtitle("side by side boxplots with marked values") +
  theme_bw()

