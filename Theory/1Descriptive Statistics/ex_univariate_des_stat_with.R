############################################################
# Univariate Descriptive Statistics with R
# identical with example_univariate_des_stat_with.RMD
#
# File: ex_univariate_des_stat_with.R
#
############################################################

# Load necessary packages and data set
# tidyverse 
library(tidyverse)
# package with data 
library(nycflights13)
# Structure of the data set
str(flights)
flights.data <- flights

############################################################
# Discrete Variable: carrier
############################################################
# Absolute and Relative Frequencies: 
# select the variable carrier of the data set flights
# count the occurencies of the different values of carrier by 
# group_by(carrier) and count() add a column for the relative 
# frequency: count(flights) gives the total number flights; 
# since it is a tibble it must be converted to an integer
# merge the name of the airlines with a left join
FreqCarrier <- 
  flights %>% 
  select(carrier) %>%
  group_by(carrier) %>%
  count() %>% 
  mutate(relfreq = n / as.integer(count(flights))) %>%
  # join the names of the airlines 
  left_join(airlines, by = "carrier") 
# change the position of columns and the name of column n
FreqCarrier <- FreqCarrier[, c(4,1:3)]
names(FreqCarrier)[3] <- "absfreq"
# display the first lines of FreqCarrier
head(FreqCarrier)

# Bar Chart of the absolute frequency: barplot() creates a bar chart
# barplot() has a lot of arguments for the design of the diagram. 
# Here are few arguments have been set explicitly. For the other 
# arguments the default values apply.
# width = 2 to show all names of the bars
barplot(# heights of the bars
  FreqCarrier$absfreq, 
  # width of the bars
  width = 2, 
  # names of the bars
  names.arg = FreqCarrier$carrier, 
  # labels the x-axis
  xlab = "Carrier", 
  # labels the y-axis
  ylab = "absolute frequency", 
  # title of the diagram
  main = "Bar Chart") 

# Pie Chart: pie() draws a pie diagram
# pie() has a lot of arguments for the design of the diagram. 
# Here are few arguments have been set explicitly. For the other 
# arguments the default values apply.
# paste concatenate vectors after converting to character
pie(FreqCarrier$absfreq,
    # labels: string aus carrier und absfreq
    labels = paste(FreqCarrier$carrier,FreqCarrier$absfreq),
    # colours to be used to fill the slices
    col = rainbow(length(FreqCarrier$carrier)),
    # title of the diagram
    main = "Pie Chart - all Carrier")

# only carrier with relfreq >= 0.03
topCarrier <- FreqCarrier %>% filter(relfreq >= 0.03) 
topCarrier %>%
  ungroup() %>%
  add_row(
    name = "other carriers", carrier = "oc",
    absfreq = sum(FreqCarrier$absfreq) - sum(topCarrier$absfreq),
    relfreq = 1 - sum(topCarrier$relfreq)
  ) %>% arrange(desc(absfreq)) -> topCarrier
topCarrier

pie(topCarrier$absfreq,
    # labels: string aus carrier und absfreq
    labels = paste(topCarrier$carrier,topCarrier$absfreq),
    # colours to be used to fill the slices
    col = rainbow(length(topCarrier$carrier)),
    # title of the diagram
    main = "Pie Chart - top Carrier")


############################################################
# Continous variable: arr_delay of United Airlines
############################################################
# select the columns arr_delay and carrier
# filter the carrier UA and arr_delay not NA
# create a frequency table
arrdelay_UA <-
  flights %>%
  select(arr_delay, carrier) %>%
  filter(!is.na(arr_delay) & carrier == "UA") 
# frequency table
freq_tab_UA <- arrdelay_UA %>%
  group_by(arr_delay) %>%
  summarise(n =n (), 
            rel = n / length(arrdelay_UA$arr_delay)) 
freq_tab_UA

# Histogram: hist() creates a histogramm
# hist() has a lot of arguments for the design of the diagram. 
# Here are few arguments have been set explicitly. For the other 
# arguments the default values apply.
# breaks one of:
#    a vector giving the breakpoints between histogram cells,
#    a function to compute the vector of breakpoints,
#    a single number giving the number of cells for the histogram,
#    a character string naming an algorithm to compute the number of cells 
# freq logical
#    if TRUE, the histogram graphic is a representation of frequencies, 
#    the counts component of the result
#    if FALSE, the histogram has a total area of one 

# Histogram with 10 equilateral classes
hist(arrdelay_UA$arr_delay,
     # 10 equilateral classes
     breaks = 10,
     # title
     main = "Histogram of arr_delay carrier UA",
     # subtitle
     sub = "10 classes of equal width",
     # labels of the axis
     xlab = "arr_delay",
     ylab = "absolute frequency")
# relative frequencies with freq = FALSE 
# mention the changed label of the y-axis!
hist(arrdelay_UA$arr_delay,
     # 10 equilateral classes
     breaks = 10,
     # total area = 1
     freq = FALSE,
     # title
     main = "Histogram of arr_delay carrier UA",
     # subtitle
     sub = "10 classes of equal width",
     # labels of the axis
     xlab = "arr_delay",
     ylab = "relative frequency")

# Histogram with classes of different width
# smallest value
delmin <- min(arrdelay_UA$arr_delay)
# biggest value
delmax <- max(arrdelay_UA$arr_delay)

hist(arrdelay_UA$arr_delay,
     # setting the bounds of the classes
     breaks = c(delmin,-30,0,30,60,delmax), 
     # total area = 1
     freq = FALSE,
     # title
     main = "Histogram of arr_delay carrier UA",
     # subtitle
     sub = "classes of different width",
     # labels of the axis
     xlab = "arr_delay",
     ylab = "")

############################################################
# Grouped frequency tables
############################################################
# The R function cut(x,breaks) divides the range of the as a 
# vector x given data into intervals given by breaks and codes 
# the values in x according to which interval they fall. The 
# leftmost interval corresponds to level one, the next leftmost 
# to level two and so on. The classes are of the form (a,b]
# cut(x, breaks, ...)
#   - x = a numeric vector which is to be converted to a factor 
#     by cutting.
#   - breaks either a numeric vector of two or more unique cut 
#     points or a single number (greater than or equal to 2) 
#     giving the number of intervals into which x is to be cut.
#   - include.lowest logical, indicating if an 'x[i]' equal to 
#     the lowest 'breaks' value should be included.
#   - labels for the levels of the resulting category. By default, 
#     labels are constructed using "(a,b]" interval notation. If 
#     labels = FALSE, simple integer codes are returned instead 
#     of a factor.

# equilateral classes
class_tab1 <-
  arrdelay_UA %>%
  # select the column arr_delay
  select(arr_delay) %>%
  # new column class (levels of the classes)
  # 10 classes with same width
  mutate(class = cut(x = arr_delay, breaks = 10)) %>%
  # count the observations in every class
  count(class) %>%
  # new column rel denoting realtive frequencies
  mutate(rel = n / length(arrdelay_UA$arr_delay))
# display the grouped frequency table
class_tab1

# classes with different width
class_tab2 <-
  arrdelay_UA %>%
  # select column arr_delay
  select(arr_delay) %>%
  # add new column class (levels of the classes
  # 5 classes with bounds delmin,-30,0,30,60,delmax,
  # the lowest value is included in the lowest class
  mutate(class = 
           cut(x = arr_delay, 
               breaks = c(delmin,-30,0,30,60,delmax),
               include.lowest = TRUE
           )) %>%
  # count the observations in every class
  count(class) %>%
  # new column rel denoting relative frequencies
  mutate(rel = n / length(arrdelay_UA$arr_delay))
# display the grouped frequency table
class_tab2

############################################################
# Empirical distribution function of arr_delay (Skywest Airlines)
############################################################
# ecdf(x) computes an empirical distribution function of by 
# x given observations

skywest <- 
  flights %>%
  filter(carrier == "OO" & !is.na(arr_delay)) %>%
  select(arr_delay)

# create the distribution function
emp_dist1 <- ecdf(skywest$arr_delay)
#  ecdf(arrdelay_UA$arr_delay)

# emp_dist1(x) = ratio of observation <= x
emp_dist1(delmin-1)
emp_dist1(-30)
emp_dist1(0)
emp_dist1(30)
emp_dist1(delmax+1)
# diagram
plot(emp_dist1, main = "Empirical Cumulative Distribution Function",
     ylab = "H(x)")

############################################################
# Measures of the arr_delay, carrier Skywest Airlines
############################################################
# Calculation of different measures directly
  
# minimum
#min(arrdelay_UA$arr_delay)
min(skywest$arr_delay)
# maximum
#max(arrdelay_UA$arr_delay)
max(skywest$arr_delay)
# mean
#mean(arrdelay_UA$arr_delay)
mean(skywest$arr_delay)
# 3 quartiles: apply type = 1 to the values according to the definition in the lecture
#quantile(arrdelay_UA$arr_delay,probs=c(0.25,0.5,0.75),type=1)
quantile(skywest$arr_delay,probs=c(0.25,0.5,0.75),type=1)
# all six statistics together
#summary(arrdelay_UA$arr_delay)
summary(skywest$arr_delay)
# variance
#var(arrdelay_UA$arr_delay)
var(skywest$arr_delay)
# standard deviation
#sd(arrdelay_UA$arr_delay)
sd(skywest$arr_delay)

# Applying the function summarise() of dplyr-package
#arrdelay_UA %>%
skywest %>%
  summarise(
    min_delay = min(arr_delay),
    q1_delay = quantile(arr_delay,probs=0.25,type=1),
    q2_delay = quantile(arr_delay,probs=0.5,type=1),
    mean_delay = mean(arr_delay),
    q3_delay = quantile(arr_delay,probs=0.75,type=1),
    max_delay = max(arr_delay),
    var_delay = var(arr_delay),
    sd_delay = sd(arr_delay)
  ) 

# Groupwise calculation of measures of arr_delay
# Select the delays of United Airlines and US Airways
delays <- flights %>%
  select(arr_delay,carrier) %>%
  filter(!is.na(arr_delay) & ((carrier == "AS") | (carrier == "YV")))
# display the first lines
delays

# Compute different measures
delays %>% 
  group_by(carrier) %>%
  summarise(
    min_delay = min(arr_delay),
    q1_delay = quantile(arr_delay,probs=0.25,type=1),
    q2_delay = quantile(arr_delay,probs=0.5,type=1),
    mean_delay = mean(arr_delay),
    q3_delay = quantile(arr_delay,probs=0.75,type=1),
    max_delay = max(arr_delay),
    var_delay = var(arr_delay),
    sd_delay = sd(arr_delay)
  )

############################################################
# Boxplot for arr_delay 
############################################################
flights %>% 
  filter(carrier == "OO") %>% 
  select(arr_delay) %>%
  boxplot(xlab="arrival_delay", ylab ="", 
          main = "Boxplot: arrival delay Skywest Airlines with observed values")
# applying ggplot()
flights %>% 
  filter(carrier == "OO") %>% 
  select(arr_delay) %>%
  ggplot() +
  geom_boxplot(aes(x=arr_delay, y=0)) +
  geom_point(aes(x=arr_delay, y=-0.5)) + 
  ggtitle("Boxplot: arrival delay Skywest Airlines with observed values") +
  xlab("arrival delay") +
  ylab("") +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line=element_blank())

############################################################
# Boxplots for arr_delay by group
############################################################
# The format is boxplot(x, data=), where x is a formula and 
# data= denotes the data frame providing the data. An example 
# of a formula is y~group where a separate boxplot for numeric 
# variable y is generated for each value of group. 

boxplot(arr_delay ~ carrier, delays,
        main = "Boxplots of arr_delay for two carriers")
# applying ggplot()
delays %>%
  ggplot( aes(x=arr_delay, fill=carrier)) +
  geom_histogram() -> gr1
delays %>%
  ggplot(aes(x = as.factor(carrier), y = arr_delay)) +
  geom_boxplot() +
  xlab("arr_delay") -> gr2

library(gridExtra)
grid.arrange(gr2, gr1, ncol = 2)

flights %>% filter(carrier == "AS" & !is.na(arr_delay)) -> x

flights %>% filter(carrier == "YV" & !is.na(arr_delay)) -> y
hist(x$arr_delay, 
     breaks = 30,
     xlab = "arr_delay", ylab = "frequency",
     main = "histogram of arr_delay for AS and YV",
     xlim = c(min(c(x$arr_delay),y$arr_delay),max(c(x$arr_delay),y$arr_delay)),
     col = rgb(1,0,0,0.5))
hist(y$arr_delay,
     breaks = 30,
     xlim = c(min(c(x$arr_delay),y$arr_delay),max(c(x$arr_delay),y$arr_delay)), 
     col = rgb(0,0,1,0.5),
     add=T)
