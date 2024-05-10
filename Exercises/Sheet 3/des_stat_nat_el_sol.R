#####################################################
# Descriptive Statistics: National Elections
# Solution
#
# File: des_stat_nat_el_sol.R
#
#####################################################
library(tidyverse)

# Results of national elections applying tibbles
nat_el <- tibble(
  res.2013 = c(0.268,0.205,0.126,0.107,0.092,0.089,0.062,0.05),
  res.2017 = c(0.341,0.257,0.047,0.048,0.086,0.084,0.074,0.062),
  party = c("CDU","SPD","AFD","FDP","Die Linke","Gruene","CSU","Others"),
  diff = res.2017 - res.2013
)
nat_el

# You can adjust the size of the margins by specifying a margin parameter
# using the syntax par(mar = c(bottom, left, top, right)), where the
# arguments bottom, left are the size of the margins. The default value
# for mar is c(5.1, 4.1, 4.1, 2.1). To change the size of the margins of a
# plot you must do so with par(mar) before you actually create the plot.
# to increase plot margins on the side of the figure.

# mfrow A vector of length 2, where the first argument specifies the
# number of rows and the second the number of columns of plots.

# cex: A numerical value giving the amount by which plotting text and
# symbols should be magnified relative to the default. This starts as 1
# when a device is opened, and is reset when the layout is changed, e.g.
# by setting mfrow.

# To ensure that large labels stay in figure we choose mar= c(2, 2, 0.5, 0.5).
# To have the plots below each other we choose mfrow = c(3,1)
# To ensure that the text of labels fits in the diagram we set cex=0.45
par(mar= c(2, 2, 0.5, 0.5), mfrow=c(3,1), cex = 0.45)

pie(results2017, labels = paste(party,"(",results2017,")"))

barplot(results2017,names.arg=party,
        ylim=c(0,0.7), xlab="Parties",ylab="2017 Votes (%)")
barplot(difference, names.arg=party,
        ylim=c(-0.1,0.1),
        xlab="Parties",ylab="Difference to 2103")

# diagrams with ggplot
# ggplot ordes the bars according to the alphabetic order of the x values, here party.
# The order can be changed by adding a factor to the variably party where the levels
# represents the newly defined order.
nat_el$party <- factor(nat_el$party, 
                       c("CDU","SPD","AFD","FDP","Die Linke","Gruene","CSU","Others"))
ggplot(data = nat_el, mapping = aes(x = "", y = results2017, fill = party)) + 
  geom_col(width = 1) +
  coord_polar(theta = "y") + 
  geom_text(mapping = aes(label = paste(party,"(",results2017*100,")")), 
            position = position_stack(vjust = 0.5)) +
  theme_void()
ggplot(data = nat_el) +
  geom_col(mapping = aes(x=party,y=results2017)) +
  xlab("Parties")+
  ylab("2017 Votes (%)") +
  theme_bw()
ggplot(data = nat_el) +
  geom_col(mapping = aes(x=party, y=diff)) +
  xlab("Parties")+
  ylab("Difference to 2013") +
  theme_bw()

