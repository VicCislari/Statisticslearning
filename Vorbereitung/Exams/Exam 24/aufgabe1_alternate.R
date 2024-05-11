library(tidyverse)

# a)
#setwd("C:\\Users\\Sunny\\OneDrive - stud.fra-uas.de\\Uni\\Statistics\\exams\\2023_WS")
#setwd("/Users/vicis/My Drive/FUAS/Lectures/Sem.8.SoSe.2024/Nachholklausuren/Statstik/Vorbereitung")
#setwd("C:\\Users\\Sunny\\OneDrive - stud.fra-uas.de\\Uni\\Statistics\\exams\\2023_WS")
#setwd("C:\\Users\\vicis\\My Drive\\FUAS\\Lectures\\Sem.8.SoSe.2024\\Nachholklausuren\\Statstik\\Vorbereitung")
#setwd(file.choose())


#df <- read.csv('europe_population_example.csv')
#df
path <- "/Users/vicis/My Drive/FUAS/Lectures/Sem.8.SoSe.2024/Nachholklausuren/Statstik/Vorbereitung/Exam 24"
file_name <- "europe_population_example.csv"

# Combine path and file name using paste()
file_path <- paste(path, file_name, sep = "/")

df <- read.csv(file_path)
df
# b)
# country: qualitativ, nominal, discrete
# years: quanti, interval, c

# c)
# Messy because years is a variable, but is spread -> tidy: country, years, poulation

df <- df %>% 
  gather(key = "years", value = "population", year_1960:year_2020) %>% 
  mutate(
    years = as.integer(str_remove(years, "year_")),
    population = as.integer(population)
  )
df

# d)
population_changes <- df %>% group_by(country) %>%
  summarise(
    pop_1960 = population[years == 1960],
    pop_2020 = population[years == 2020],
    abs_change = abs(pop_2020 - pop_1960), # negative Änderung absolut
    rel_change = abs_change / pop_1960 * 100
  ) %>%
  arrange(desc(abs_change))

population_changes

top_5_abs_change <- head(population_changes, 5)
bottom_5_abs_change <- tail(population_changes, 5)
top_5_rel_change <- population_changes %>% arrange(desc(rel_change)) %>% head(5)
bottom_5_rel_change <- population_changes %>% arrange(rel_change) %>% head(5)

top_5_abs_change
bottom_5_abs_change
top_5_rel_change
bottom_5_rel_change

# e)

daten <- df %>% filter(country == "Germany")
plot(
  daten$years, 
  daten$population, 
  main = "Deutsche Population 1960-2020",
  xlab = "Jahre",
  ylab = "Population"
)
daten
modell <- lm(daten$population ~ daten$years, data = daten)
abline(modell, col = 'red', lwd = 2)
modell
# f)
a <- modell$coefficients[1]
b <- modell$coefficients[2]
a # intercept 475549720 Y-Achsenabschnitt
b # daten$years -212961.6 Steigung

# g)
summary(modell)$r.squared # # R² des Modells
# 0.01725374 says nothing
# 

# h)
year = 2023
y = a + b * year
y

# oder ?

y <- predict(modell, newdata = data.frame(years = year))
y