###################################################################################
# Magnets and Pain Relief
# Case Study: https://onlinestatbook.com/case_studies_rvls/magnets/index.html
#
#
# file: case_study_magnetes_pain_sol.R
###################################################################################

library(tidyverse)

# Is it possible that magnetic fields can reduce pain?

# Load the raw data
raw.data <- read_csv2(
  "C:/Users/Egbert Falkenberg/Nextcloud/NextCloud Lehre/aktuelle_LV/Statistik/Exercises/R_Solutions/Sheet_9/magnets_pain.csv") 

# side-by-side-boxplots
boxplot(Score_2 ~ Active, data = raw.data, 
        names = c("Treatment", "Control"), 
        ylab = "Pain Rating", xlab = "")

# The plot shows pain ratings of subjects treated with active magnets (Treatment)
# and those treated with inactive placebo devices (Control).

# What is the mean, min, max, Q1, Q2, Q3 and standard deviation for the group 
# treated with active magnets? 
raw.data %>%
  group_by(Active) %>%
  summarise(Mean = mean(Score_2), Min = min(Score_2), Max = max(Score_2),
            Q1 = quantile(Score_2, prob = 0.25), Median = median(Score_2),
            Q3 = quantile(Score_2, prob = 0.75), IQR = Q3-Q1, SD = sd(Score_2))

# How do you interpret the boxplots and these characteristic numbers?
# Answers:
# a) The lowest pain rating of the control group appears to be equal to the median 
#    of the treatment group
# b) Overall, the group in the most pain is the control group.
# c) The middle 50% of the scores for the control group fall between 8-10.
# d) The circle represents an outlier.
# e) The range for the group that received the active magnets is 10.
# f) The pain ratings are more variable in the treatment condition.

# Inferential Statistics: t-test on post-treatment scores
# One way to test the hypothesis that magnets reduce pain is to test the null 
# hypothesis that there is no difference in post-treatment ratings of pain. If 
# this null hypothesis can be rejected, then it can be concluded that there is 
# an effect of treatments, i.e. a difference in ratings between those treated 
# with active magnets and those treated with placebo magnets. 
# An independent-groups t-test can be used for this. This test makes three assumptions:
# -) The populations are each normally distributed.
# -) The variances in the populations are equal.
# -) Each observation is sampled randomly and is therefore independent of each 
#    other observation. 

# Here we assume that the populations are normally distributed.

# The active magnet group ratings were more variable (sd= 3.14) then were the 
# ratings of the placebo group (sd = 1.86). An F test of the difference in 
# variances can be computed by dividing the variance of the group with the larger
# variance by the variance of the group with the smaller variance. 

treatment <- raw.data %>% filter(Active == 1) 
control <- raw.data %>% filter(Active == 2) 
var.test(
  x = treatment$Score_2, y = control$Score_2,
  alternative = "greater"
)
# 	F test to compare two variances
# data:  treatment$Score_2 and control$Score_2
# F = 2.8598, num df = 28, denom df = 20, p-value = 0.008893
# alternative hypothesis: true ratio of variances is greater than 1
# 95 percent confidence interval:
#   1.393891      Inf
# sample estimates:
#   ratio of variances 
# 2.859789 

# The variances are significantly different.

# Results of a 2-sample, unpaired t-test (Welch-test)
t.test(
  x = treatment$Score_2, y = control$Score_2,
  alternative = "two.sided", mu = 0,
  paired = FALSE, var.equal = FALSE
)
# Welch Two Sample t-test
# data:  treatment$Score_2 and control$Score_2
# t = -5.695, df = 46.418, p-value = 8.058e-07
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -5.480119 -2.618404
# sample estimates:
#   mean of x mean of y 
# 4.379310  8.428571 

# Interpretation: Subjects who had an active magnet reported significantly less 
# pain than did subjects who recieved the placebo. The confidence interval on the 
# difference between means shows that the effect is large and therefore of practical
# as well as statistical significance.
