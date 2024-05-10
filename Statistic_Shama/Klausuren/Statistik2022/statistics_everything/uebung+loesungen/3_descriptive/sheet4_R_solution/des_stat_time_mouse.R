#####################################################
# Descriptive Statistics: Times to move the mouse
#
# File: des_stat_time_mouse.R
#
#####################################################
library(tidyverse)
library(xtable) # only necessary to get a tex-table

# 4) The data shown in the list are the times in 
# milliseconds it took one of us to move the mouse 
# over a small target in a series of 20 trials. 
# The times are sorted from shortest to longest.

times <- c(568, 577, 581, 640, 641, 645, 657, 673, 696, 
           703, 720, 738, 729, 777, 808, 824, 825, 865,
           975, 1007)

#####################################################
# ungrouped data                                    #
#####################################################
df <- tibble(values = times) %>%
  # count the number of observations per observed value
  group_by(values) %>%
  mutate(
    abs.freq = n()
    ) %>%
  unique() %>%  # remove multiple entries
  ungroup() %>% # remove group by to regard all observations
  mutate(
    rel.freq = abs.freq / sum(abs.freq),
    cum.rel.freq = cumsum(rel.freq)
    )
df

# alternative solution
H <- ecdf(times)  # emp. cum. distr. function
tibble(obs.values = knots(H),      # -> observation values
       cum.rel.freq = H(obs.values)) %>%
  mutate(
    # for the smallest obs. value is cum.rel.freq = rel. freq
    # lag() -> value before the current value
    rel.freq = if_else(is.na(lag(cum.rel.freq)),cum.rel.freq, 
                       cum.rel.freq - lag(cum.rel.freq)),
    abs.freq = rel.freq * length(times)  # length(times) = no of of obs.
  )

# another alternative solution
tab <- table(times)
tibble(
  values = tab %>% names() %>% as.numeric(),
  abs.freq = tab %>% as.integer(),
  rel.freq = abs.freq / length(times) ,
  cdf = cumsum(rel.freq)
)

# b) Compute and draw the cumulative frequency distribution.
H <- ecdf(times)

# tex Tabelle erzeugen
xtable(df_tab[,c(1,4)]) %>% print(include.rownames = FALSE, floating = FALSE)

# emp. Verteilungsfkt.
plot.ecdf(times,
          xlab = "time", ylab = "H(x)", 
          main = "ungrouped data")
# eps-file
dev.copy2eps(file="../pictures/time_emp_dis1.eps")

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

cut(times, breaks = bounds    )

times_cut <- cut(times, breaks = bounds,
                 # labels denotes the names of values
                 # default: classes like (500,60], ...
                 # here: value = upper bound of the class
                 labels = bounds[-1]) # leave the first value

# cut(times, breaks = bounds) # labels are the classes (a,b]

df_cut <-
  tibble(upper_bound = times_cut) %>%
  group_by(upper_bound) %>%
  mutate(n = n(),
         rel = n / length(times)) %>%
  ungroup() %>%
  unique() %>%  # remove multiple entries
  mutate(cum_rel_freq = cumsum(rel))
df_cut


# alternative solution using the emp. cum. dist. function H
tibble(obs.values = bounds[-1],      # -> upper bounds of the classes
       cum.rel.freq = H(obs.values)  # cum. rel. freq. of the classes
       ) %>%
  mutate(
    # for the smallest obs. value is cum.rel.freq = rel. freq
    # lag() -> value before the current value
    rel.freq = if_else(is.na(lag(cum.rel.freq)),cum.rel.freq, 
                       cum.rel.freq - lag(cum.rel.freq)),
    abs.freq = rel.freq * length(times)  # length(times) = no of of obs.
  )

# tex Tabelle erzeugen
xtable(df_cut_tab) %>% print(include.rownames = FALSE, floating = FALSE)

# Compute the grouped frequency distribution and draw the histogram.
# Histogramm
hist(times, breaks = bounds, xlab = "time")

# eps-file
dev.copy2eps(file="../pictures/time_hist.eps")

# remark: coerce the values of times_cut to character and
# then to integer to get integer values!!!
# H based on grouped data
plot.ecdf(as.integer(as.character(times_cut)),
          xlab = "times", ylab = "F(x)", 
          main = "F based on grouped data")

# eps-file
dev.copy2eps(file="d:/Lehre/MATHE/Statistik/Exercises/pictures/time_emp_dis2.eps")

# H based on grouped data under the assumption of uniformly
# distributed values in the classesd
plot(x = c(500,as.integer(as.character(df_cut$upper_bound))),
     y = c(0,df_cut$cum_rel_freq),
     type = "b",
     xlab = "time", ylab = "",
     main = "H - grouped data",
     sub = "Assumption: all values are uniformly distributed in the classes")

# eps-file
dev.copy2eps(file="../pictures/times_emp_dis3.eps")

# empirical distribution function based on grouped data
# assumming uniformly distributed values in the classes
xval <- c(500,as.integer(as.character(df_cut$upper_bound)))
yval <- c(0,df_cut$cum_rel_freq)
H_gr_uni <- function(x,xval,yval) {
  if ( x < xval[1]) {
    return(0)
  } else {
    if (x > max(xval)) {
      return(1)
    } else {
      i <- max(which(x >= xval))
      return(yval[i] + (yval[i+1]-yval[i])*(x-xval[i])/(xval[i+1]-xval[i]))
    }
  }
}

# Compute the proportion of response times
# less equal 800
H_gr_uni(800,xval, yval) # 0.7
# greater than 725
1-H_gr_uni(725,xval, yval) # 0.4875
# greater than 642 and less equal 777
H_gr_uni(777,xval, yval) - H_gr_uni(642,xval, yval) # 0.3665
# equal 696 --> Grenzwert: 0

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
  tibble(upper_bound = times_cut) %>%
  group_by(upper_bound) %>%
  count() %>%
  mutate(rel = n / length(times)) %>%
  ungroup() %>%
  mutate(cum_rel_freq = cumsum(rel))
df_cut_diff

# alternative solution using the emp. cum. dist. function H
tibble(obs.values = bounds[-1],      # -> upper bounds of the classes
       cum.rel.freq = H(obs.values)  # cum. rel. freq. of the classes
) %>%
  mutate(
    # for the smallest obs. value is cum.rel.freq = rel. freq
    # lag() -> value before the current value
    rel.freq = if_else(is.na(lag(cum.rel.freq)),cum.rel.freq, 
                       cum.rel.freq - lag(cum.rel.freq)),
    abs.freq = rel.freq * length(times)  # length(times) = no of of obs.
  )


# tex Tabelle erzeugen
xtable(df_cut_tab) %>% print(include.rownames = FALSE, floating = FALSE)

# Histogramm
hist(times, breaks = bounds, xlab = "time", ylab ="")

# eps-file
dev.copy2eps(file="../pictures/time_hist2.eps")

