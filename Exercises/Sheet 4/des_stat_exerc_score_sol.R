#####################################################
# Descriptive Statistics: time spent in exercise and
#                         score in exam
# Solution
# File: des_stat_exerc_score_sol.R
#
#####################################################
# load package
library(tidyverse)

# For a certain class, the relationship between the 
# amount of time spent in exercises (X) and the test 
# score (Y) was examined.
results <- 
  tibble(
    time =  c(10,9,9,11,10,10,6,10,8,12,9,4,12),
    score = c(5, 5,4,6, 7, 5, 3,4, 5,7, 4,2,8)
  )

# a) Draw a scatterplot of the data.
plot(x = results$time, 
     y = results$score,
     main="time spent in exercises and score",
     xlab="time spent", ylab="score")
# applying ggplot()
ggplot(data = results) +
  geom_point(mapping = aes(x=time, y=score)) +
  ggtitle("relationship: time spent in exercises and score") +
  theme_bw()

# Compute the covariance and the coefficient of correlation.
results %>%
  summarise("covariance" = cov(time,score),
            "coeff. of correlation" = cor(time,score))

# Compute the regression line Y=a+bX.(X = time, Y = score)
reg1 <- lm(results$score~results$time)
# coefficients of the line
a <- reg1$coefficients[1]
b <- reg1$coefficients[2]
a;b

# add the residuals and the fitted values to the tibble results
results <- results %>%
  mutate(res.scores = reg1$residuals,
         pred.scores = reg1$fitted.values)
results

# Find the predicted score for someone with 8 units of 
# time spent in exercises.
pred_test_score <- a+b*8
pred_test_score #  4.204082

plot(x = results$time, 
     y = results$score,
     xlim = c(-1,13),
     ylim = c(-1,10),
     main="time spent in exercises and score",
     xlab="time spent", ylab="score")
# add the regression line
abline(reg1, col="blue")
# add line segments
segments(8,pred_test_score,0,pred_test_score)
segments(8,0,8,pred_test_score)
# add axis
abline(h=0)
abline(v=0)

# applying ggplot()
ggplot(data = results) + 
  geom_point(mapping = aes(x=time, y=score)) +
  geom_smooth(mapping = aes(x=time, y=score), method = "lm", 
              se = FALSE, color = "blue") + 
  geom_segment(mapping = aes(x=time, y=score, xend=time, yend=pred.scores)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  ggtitle("relationship: time spent in exercises and score") +
  theme_bw()

# Compute the proportion of variation explained by the linear reg.
B <- cor(results$time,results$score)^2
B #  0.7417842

# add the point (20,0)
results <-
  results %>%
  add_row(time=20,score=0)

# calculate again the regression line
reg2 <- lm(results$score~results$time)
# blot both regression lines
plot(x = results$time, 
     y = results$score,
     xlim = c(-1,21),
     ylim = c(-1,10),
     main="time spent in exercises and score",
     sub="blue=regr. without (20,0),red=regr. with (20,0)",
     xlab="time spent", ylab="score")
# add regression lines
abline(reg2, col="red")
abline(reg1,col="blue")
# add axis
abline(h=0)
abline(v=0)

# applying ggplot()
ggplot(data = results) + 
  geom_point(mapping = aes(x=time, y=score)) +
  geom_smooth(mapping = aes(x=time, y=score), method = "lm", 
              se = FALSE, color = "blue") + 
  # changes caused by new point (20,0)
  geom_smooth(data = results %>% add_row(time=20,score=0),
              mapping = aes(x=time, y=score), method = "lm", 
              se = FALSE, color = "red") + 
  geom_point(x=20,y=0, color="red") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  ggtitle("relationship: time spent in exercises and score",
          subtitle = "red changes caused by new point") +
  theme_bw()

# coeffiecient of determination
B_new <- cor(results$time,results$score)^2
B_new
