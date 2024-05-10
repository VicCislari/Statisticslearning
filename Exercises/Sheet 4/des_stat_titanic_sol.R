#####################################################
# Descriptive Statistics: Exercise 4.4, 
# Heumann, Schomaker, page 91
# Solution
#
# File: des_stat_titanic_sol.R
###################################################### 
# load packages
library(tidyverse)
library(titanic)

# use of the given values !!!!!!!
# generate raw data
raw_data <-
  tibble(
    "class" = c(rep("first",325),
                rep("second",285),
                rep("third",706),
                rep("staff",885)),
    "state" = c(rep("not rescued",122),rep("rescued",203),
                rep("not rescued",167),rep("rescued",118),
                rep("not rescued",528),rep("rescued",178),
                rep("not rescued",673),rep("rescued",212))
  )
# number of observations 
nobs <- length(raw_data$class)
nobs
# contingency table
raw_data %>% table()

# generate the data from the titanic data set
tdata <- 
  as.tibble(Titanic) %>% 
  spread(key=Survived, value=n) %>%
  select(-Age, -Sex) %>% 
  group_by(Class) %>% 
  summarise(not.rescued = sum(No),
            rescued = sum(Yes)) %>%
  mutate(Sum = not.rescued+rescued)
tdata  

# conditional frequencies rescue status given class
tdata %>%
  mutate(
    not.rescued = not.rescued / Sum,
    rescued = rescued / Sum
  ) %>%
  select(-Sum) -> cond_freq
cond_freq$rescued # proportion of the rescued persons depending on the class

# use of chisq.test() to get the indifference table and chi2
tdata %>%
  select(not.rescued, rescued) %>%
  # chisq.test() needs a matrix as an input
  as.matrix() %>% 
  chisq.test() -> test.res 

# indifference table
test.res$expected %>% addmargins()

# chi-square
test.res$statistic -> chi2
chi2

# Pearson's contingency coefficient
nobs <- tdata$Sum %>% sum()
C <- (chi2/(chi2+nobs))**0.5
C

# corrected Pearson's contingency coefficient
C_cor <- (2/1 * chi2/(chi2+nobs))**0.5
C_cor

###############################################################
# group first and second class as well as third class and staff
###############################################################
raw_data %>%
  mutate(grouped.class = if_else(class == "first" | class == "second", "first+second","third+staff")) %>%
  count(grouped.class, state) %>%
  spread(key = state, value = n) 
# or
cont_tab_new <-
  tibble(
    Class = c("first+second","third+staff"),
    not.rescued = c(tdata$not.rescued[1]+tdata$not.rescued[2],
                    tdata$not.rescued[3]+tdata$not.rescued[4]),
    rescued = c(tdata$rescued[1]+tdata$rescued[2],
                    tdata$rescued[3]+tdata$rescued[4])
  )
cont_tab_new

# indifference table
chisq.test(
  cont_tab_new %>% select(-Class) %>% as.matrix()
)$expected %>% addmargins()

# chi-square
chi2_new <- 
  chisq.test(
    cont_tab_new %>% select(-Class) %>% as.matrix()
  )$statistic
chi2_new

# Pearson's contingency coefficients
C_new <- (chi2_new/(chi2_new+nobs))**0.5
C_new
C_cor_new <- (2/1 * chi2_new/(chi2_new+nobs))**0.5
C_cor_new

# Chi^2 Test, p-value < 2.2e-16
chisq.test(
  cont_tab_new %>% select(-Class) %>% as.matrix()
)

# conditional frequencies: rescue status given new classes
cond_freq_new <- 
  tibble(
    Class = cont_tab_new$Class,
    not.rescued = cont_tab_new$not.rescued / 
      (cont_tab_new$not.rescued + cont_tab_new$rescued),
    rescued = cont_tab_new$rescued / 
      (cont_tab_new$not.rescued + cont_tab_new$rescued),
  )
cond_freq_new
cond_freq_new$rescued # proportion of the rescued persons depending on the class

# relative risks
rel_risks <- c(
  cond_freq_new$not.rescued[1] / cond_freq_new$not.rescued[2],
  cond_freq_new$rescued[1] / cond_freq_new$rescued[2]
)
# proportion of not rescued persons among 1./2. class when compared with 3.class and staff
rel_risks[1]
# proportion of rescued persons among 1./2. class when compared with 3.class and staff
rel_risks[2] 

# odds ratio
rel_risks[2] / rel_risks[1] # chance to be rescued for 1./2. class when compared with 3.class and staff