#####################################################
# Descriptive Statistics: miles per gallon
#
# File: des_stat_miles_gallon.R
#
######################################################
# load tidyvers
library(tidyverse)

# inspect the description of the data set
?mpg()

# select only the variables displ and hwy and add 
# a colummn displ_class which denotes the belonging
# to one of the groups 
# low (1 <= displ < 3), medium (3 <= displ < 5),
# big (5 <= displ < 8)
tab <-
  mpg %>%
  select(displ,hwy) %>%
  mutate(displ_class = 
           cut(displ,breaks = c(1,3,5,8),
               labels = c("small","medium","big"))
  )
tab
# tex table
tex_tab <- xtable(tab)
print(tex_tab, include.rownames = FALSE, floating = FALSE)

# calculate mean, min, max Q1, Q2 and Q3 of the variable
# hwy grouped by the values of displ.
stat_hwy_displ <-
  tab %>%
  group_by(displ) %>%
  summarise(mean(hwy),min(hwy),max(hwy),
            q1=quantile(hwy,0.25, type=2),
            q2=quantile(hwy,0.5, type=2),
            q3=quantile(hwy,0.75, type=2)
            )
stat_hwy_displ
# tex table
tex_tab <- xtable(stat_hwy_displ)
print(tex_tab, include.rownames = FALSE, floating = FALSE)

# calculate mean, min, max Q1, Q2 and Q3 of the variable
# hwy grouped by the values of displ_class.
stat_hwy_class <-
  tab %>%
  group_by(displ_class) %>%
  summarise(mean(hwy),min(hwy),max(hwy),
            q1=quantile(hwy,0.25, type=2),
            q2=quantile(hwy,0.5, type=2),
            q3=quantile(hwy,0.75, type=2)
  )
stat_hwy_class
# tex table
tex_tab <- xtable(stat_hwy_class)
print(tex_tab, include.rownames = FALSE, floating = FALSE)

# boxplots of hwy grouped by displ
par(mfrow = c(1,1))
boxplot(hwy ~ displ, data = tab, xlab = "displ", ylab = "hwy")
# using ggplot
#ggplot(data = tab) +
#  geom_boxplot(
#    mapping = aes(group = displ, x = displ, y = hwy)
#  )
# eps files
dev.copy2eps(file = "../pictures/miles_gallon1.eps")

# boxplots of hwy grouped by displ_class
boxplot(hwy ~ displ_class, data = tab, xlab = "displ_class", ylab = "hwy")
# using ggplot
#ggplot(data = tab) +
#  geom_boxplot(
#    mapping = aes(group = displ_class, 
#                  x = displ_class, y = hwy)
#  )
# eps files
dev.copy2eps(file = "../pictures/miles_gallon2.eps")

# both boxplots together
par(mfrow = c(1,2))
boxplot(hwy ~ displ, data = tab, xlab = "displ", ylab = "hwy")
boxplot(hwy ~ displ_class, data = tab, xlab = "displ_class", ylab = "hwy")
 # zurücksetzen von mfrow
par(mfrow = c(1,1))