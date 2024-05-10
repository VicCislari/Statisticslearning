#####################################################
# Descriptive Statistics: Exercise 4.4, 
# Heumann, Schomaker, page 91
#
# File: des_stat_titanic.R
#
###################################################### 

# load packages
library(tidyverse)
library(xtable) # necessary to generate tex tables
library(titanic)

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

# contingency table
cont_tab <- 
  raw_data %>%
  table() 
# display
cont_tab
# add margins 
addmargins(cont_tab) 


# tex table
tex_tab <- xtable(addmargins(cont_tab))
print(tex_tab, include.rownames = TRUE, floating = FALSE)

# conditional frequencies rescue status given class
cond_freq <- cont_tab / rowSums(cont_tab)
cond_freq[,2] # proportion of the rescued persons depending on the class

# tex table
tex_tab <- xtable(cond_freq)
print(tex_tab, include.rownames = TRUE, floating = FALSE)

# indifference table
ind_tab <- as.matrix(rowSums(cont_tab)) %*% 
           t(as.matrix(colSums(cont_tab))) / sum(cont_tab)
# display
ind_tab
# add margins
addmargins((ind_tab))

# tex table
tex_tab <- xtable(ind_tab)
print(tex_tab, include.rownames = TRUE, floating = FALSE)

# chi-square
chi2 <- sum((cont_tab-ind_tab)^2 / ind_tab)
chi2

# directly applying chisq.test
chisq.test(x = raw_data$class, y=raw_data$state)$statistic

# A simple solution to evaluate the contingency table, the indifference table
# and Chi^2 is applying chisq.test() to the raw_data
test.res <- chisq.test(x = raw_data$class, y = raw_data$state)
test.res$statistic # chi^2
test.res$observed %>% addmargins()  # contingency table
test.res$expected %>% addmargins() # indifferennce table

# Pearson's contingency coefficient
C <- (chi2/(chi2+nobs))**0.5
C

# corrected Pearson's contingency coefficient
C_cor <- (2/1 * chi2/(chi2+nobs))**0.5
C_cor

###############################################################
# group first and second class as well as third class and staff
###############################################################
cont_tab_new <-
  raw_data %>% 
  mutate("class" = if_else(class == "first" | class == "second", 
                           "first+second", "third+staff")) %>%
  table()
# display 
cont_tab_new
# add margins
addmargins(cont_tab_new)

# tex table
tex_tab <- xtable(cont_tab_new)
print(tex_tab, include.rownames = TRUE, floating = FALSE)


# indifference table
ind_tab_new <-
  as.matrix(rowSums(cont_tab_new)) %*% 
  t(as.matrix(colSums(cont_tab_new))) / sum(cont_tab_new)
# display
ind_tab_new
# add margins
addmargins(ind_tab_new)

# tex table
tex_tab <- xtable(ind_tab_new)
print(tex_tab, include.rownames = TRUE, floating = FALSE)

# chi-square
chi2_new <- sum((cont_tab_new-ind_tab_new)^2 / ind_tab_new)
chi2_new

# Pearson's contingency coefficients
C_new <- (chi2_new/(chi2_new+nobs))**0.5
C_new
C_cor_new <- (2/1 * chi2_new/(chi2_new+nobs))**0.5
C_cor_new

# Chi^2 Test, p-value < 2.2e-16
chisq.test(cont_tab_new)

# conditional frequencies: rescue status given new classes
cond_freq_new <- cont_tab_new / rowSums(cont_tab_new)
cond_freq_new
cond_freq_new[,2] # proportion of the rescued persons depending on the class

# relative risks
rel_risks <- cond_freq_new[1,] / cond_freq_new[2,]
rel_risks[1] # proportion of not rescued persons among 1./2. class when compared with 3.class and staff
rel_risks[2] # proportion of rescued persons among 1./2. class when compared with 3.class and staff

# odds ratio
rel_risks[2] / rel_risks[1] # chance to be rescued for 1./2. class when compared with 3.class and staff