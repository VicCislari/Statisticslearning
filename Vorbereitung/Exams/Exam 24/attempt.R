# AUFGABE 4
# (a)
path <- "/Users/vicis/My Drive/FUAS/Lectures/Sem.8.SoSe.2024/Nachholklausuren/Statisticslearning/Vorbereitung/Exam 24"

data <- read.csv(paste(path, "/runs.csv", sep = ""))
n <- 186 #participants
# (b)
# Calculate the 95% confidence interval
t.test(data$time, conf.int = 0.95)$conf.int# (c)
# 95% of the data (centered at the median)
# is within the values of 97.6 and 101.6# (d)
# No idea what he means by that 4....

len=4
sd <- sd(data$time)
u <- (len*sqrt(n))/(2*sd)
u
conf_leve <- (1-(2*(1-pt (u,df=n-1))))
conf_leve
# (e)
# One year ago the mean of the time was 98.54.
# Conduct an appropriate statistical test on
# a 5 percent level to check whether the expected
# value is larger than last year.
# Specify the null hypthesis.
# What is the p-value?
# What is your decision?

# Load the data (assuming 'time_now' contains the current time data)
time_now <- data$time

# Set significance level (alpha)
alpha <- 0.05 # default, because no specific instruction in exercise

# Define the mean time from one year ago
mean_last_year <- 98.54

# Null hypothesis (H0): mean_now <= 98.54 (no significant change or decrease)
# Alternative hypothesis (H1): mean_now > 98.54 (increase)

# Print the null and alternative hypotheses
cat("Null hypothesis (H0): mean_now <= 98.54\n")
cat("Alternative hypothesis (H1): mean_now > 98.54\n\n")

cat("Null hypothesis (H0): 98.54 => mean_now \n")
cat("Alternative hypothesis (H1): 98.54 < mean_now \n\n")

# APPROXIMATION
cat("--- Approximation Method ---\n")

# Calculate the test statistic for the approximation method
test_stat_approx <- (mean(time_now) - mean_last_year) / (sd(time_now) / sqrt(length(time_now)))
cat("Test Statistic (Approximation Method):", test_stat_approx, "\n")

# Calculate the critical value
t_approx <- qt(1 - alpha, df = length(time_now) - 1)
cat("Critical Value (t) for Approximation Method:", t_approx, "\n")

# Determine whether to reject H0 based on approximation
reject_H0_approx <- test_stat_approx > t_approx
cat("Reject H0 based on Approximation Method:", reject_H0_approx, "\n")

# Calculate the p-value based on approximation
p_value_approx <- 1 - pt(test_stat_approx, df = length(time_now) - 1)
cat("P-value based on Approximation Method:", p_value_approx, "\n\n")

# EXACT
cat("--- Exact Method ---\n")

# Perform one-sample t-test for the exact method
# x ist my sample
# mu ist 98.54
t_test_result_exact <- t.test(x = time_now, mu = mean_last_year, alternative = "less", conf.level = 1 - alpha)

# Extract test statistic, p-value, and other relevant information
test_stat_exact <- t_test_result_exact$statistic
p_value_exact <- t_test_result_exact$p.value
reject_H0_exact <- p_value_exact < alpha

# Print the test statistic and p-value for the exact method
cat("Test Statistic (Exact Method):", test_stat_exact, "\n")
cat("Reject H0 based on Exact Method:", reject_H0_exact, "\n")
cat("P-value based on Exact Method:", p_value_exact, "\n")

#e
#h0: 98.54>=mu, h1: 98.54<mu
n
mean=98.54
alpha=0.05
mu <- mean(data$time)
mu
s <- sd(data$time)
s
# pvalu=0.14 which means that
# the h0 is  rejected and
# the value is larger than last year
t=(mu-mean)/(s/sqrt(n))
t
t>qnorm(1-alpha)
pval<-1-pnorm(t)
pval