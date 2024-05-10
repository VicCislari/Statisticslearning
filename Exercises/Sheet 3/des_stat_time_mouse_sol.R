#####################################################
# Descriptive Statistics: Times to move the mouse
# Solution
#
# File: des_stat_time_mouse_sol.R
#
#####################################################
library(tidyverse)

# 4) The data shown in the list are the times in 
# milliseconds it took one of us to move the mouse 
# over a small target in a series of 20 trials. 
# The times are sorted from shortest to longest.

times <- c(568, 577, 581, 640, 641, 645, 657, 673, 696, 
           703, 720, 728, 729, 777, 808, 824, 825, 865,
           975, 1007, 1007)

#####################################################
# ungrouped data                                    #
#####################################################
# solution applying count()
df <- tibble(values = times) %>%
  # count the number of observations per observed value
  count(values) %>%
  mutate(
    abs.freq = n,
    rel.freq = abs.freq / sum(abs.freq),
    cum.rel.freq = cumsum(rel.freq)
  ) %>%
  select(-n)
df

# b) Compute and draw the cumulative frequency distribution.
H <- ecdf(times)
H(700)

# emp. Verteilungsfkt.
plot.ecdf(times,
          xlab = "time", ylab = "H(x)", 
          main = "ungrouped data")

# plot the empirical distribution function with ggplot()
ggplot(data = 
         df %>% 
         mutate(x1 = values, x2 = c(values[-1],100+max(values)))) +
  geom_point(mapping = aes(x=values, y=cum.rel.freq)) + 
  geom_segment(mapping = aes(x = x1, y = cum.rel.freq, 
                             xend = x2, yend = cum.rel.freq)) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = min(df$values)-100) +
  xlab("time") +
  ylab("H(x)") +
  ggtitle("empirical distribution function") +
  theme_classic()

# c) Compute the proportion of response times
# less equal 800
H(800) # 0.7
# greater than 725
1-H(725) # 0.45
# greater than 642 and less equal 777
H(777) - H(642) # 0.45
# equal 696 --> Grenzwert
# H(696) - H(695) # 0.05
sum(df$values == 696)/length(df$values)
# in [698, 800]
H(800)-H(696)+sum(df$values == 696)/length(df$values)


#####################################################
# grouped data                                      #
#####################################################
# Consider the following classes 
# (500,600],(600,700],(700,800],(800,900],(900,1000],
# (1000,1100]
# classbounds:
bounds <- c(500,600,700,800,900,1000,1100)

cut(times, breaks = bounds)

times_cut <- cut(times, breaks = bounds,
                 # labels denotes the names of values
                 # default: classes like (500,60], ...
                 # here: value = upper bound of the class
                 labels = bounds[-1]) # leave the first value

# cut(times, breaks = bounds) # labels are the classes (a,b]

# solution applying count()
df_cut <- 
  tibble(upper_bound = 
           # convert factor times cut to numeric
           times_cut %>% as.character() %>% as.numeric()) %>%
  count(upper_bound) %>%
  mutate(rel = n / length(times_cut),
         cum.rel.freq = cumsum(rel)) 

# Compute the grouped frequency distribution and draw the histogram.
# Histogramm
hist(times, breaks = bounds, xlab = "time")

# histogram plot applying ggplot()
ggplot(data = df) +
  geom_histogram(mapping = aes(x = values), breaks = bounds, 
                 color = "grey") + 
  theme_classic() +
  ggtitle("Histogram of times")

# plot the distrubtion function
# remark: coerce the values of times_cut to character and
# then to integer to get integer values!!!
# H based on grouped data
plot.ecdf(as.integer(as.character(times_cut)),
          xlab = "times", ylab = "F(x)", 
          main = "F based on grouped data")

# plot the distribution function with ggplot()
ggplot(data = 
         df_cut %>% 
         mutate(x1 = upper_bound, x2 = c(upper_bound[-1],100+max(upper_bound)))) +
  geom_point(mapping = aes(x=upper_bound, y=cum.rel.freq)) + 
  geom_segment(mapping = aes(x = x1, y = cum.rel.freq, 
                             xend = x2, yend = cum.rel.freq)) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = min(df_cut$upper_bound)-100) +
  xlab("time") +
  ylab("H(x)") +
  ggtitle("empirical distribution function based on classes") +
  theme_classic()

#####################################################
# grouped data: different class widths              #
#####################################################
# Consider the following classes 
# (500,600],(600,900],(900,1100]
# classbounds:
bounds <- c(500,600,900,1100)
times_cut <- cut(times, breaks = bounds,
                 # labels denotes the names of values
                 # here: value = upper bound of the class
                 labels = bounds[-1])
times_cut

# cut(times, breaks = bounds) # labels are the classes (a,b]

df_cut_diff <-
  tibble(upper_bound = 
           # convert factor times cut to numeric
           times_cut %>% as.character() %>% as.numeric()) %>%
  count(upper_bound) %>%
  mutate(rel = n / length(times),
         cum.rel.freq = cumsum(rel))
df_cut_diff

# Histogram
hist(times, breaks = bounds, xlab = "time", ylab ="")
# histogram applying ggplot()
ggplot(data = df) +
  geom_histogram(mapping = aes(x = values), breaks = bounds, 
                 color = "grey") + 
  theme_classic() +
  ggtitle("Histogram of times - new bounds") +
  xlab("time") +
  ylab("")

# Since the class widths are not equal, there is non meaning full interpretation 
# of the y-values.

# plot the distribution function with ggplot()
ggplot(data = 
         df_cut_diff %>% 
         mutate(x1 = upper_bound, x2 = c(upper_bound[-1],100+max(upper_bound)))) +
  geom_point(mapping = aes(x=upper_bound, y=cum.rel.freq)) + 
  geom_segment(mapping = aes(x = x1, y = cum.rel.freq, 
                             xend = x2, yend = cum.rel.freq)) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = min(df_cut$upper_bound)-100) +
  xlab("time") +
  ylab("H(x)") +
  ggtitle("empirical distribution function based on classes with different width") +
  theme_classic()
