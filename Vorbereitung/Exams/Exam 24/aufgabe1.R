path <- "/Users/vicis/My Drive/FUAS/Lectures/Sem.8.SoSe.2024/Nachholklausuren/Statstik/Vorbereitung/Exam 24"
file_name <- "europe_population_example.csv"

# Combine path and file name using paste()
file_path <- paste(path, file_name, sep = "/")

# Read the CSV file using the constructed file path
data <- read.csv(file_path)
# Install and load the tidyr package
install.packages("tidyr")
library(dplyr)
library(tidyr)
library(ggplot2)


# (c) Solution

# first off the columns and rows and mixed up.
# it is better to habe column country, year and score -> spread

# Assuming your data is stored in a data frame called "data"
# Replace "data" with the name of your actual data frame if different
# Tidy the data and mutate the year column
tidy_data <- gather(data, year, population, -country) %>%
  mutate(year = gsub("year_", "", year))

# Display the first few rows of the updated tidy data
head(tidy_data)


# (d)

# Load necessary libraries
# Assuming your data is stored in a data frame called "data"
# Replace "data" with the name of your actual data frame if different

# Arrange data by country and year
arranged_data <- tidy_data %>% arrange(country, year)

# Calculate absolute change
# https://www.youtube.com/watch?v=TGFTUibUquQ
arranged_data <- arranged_data %>%
  group_by(country) %>%
  mutate(
    absolute_change = abs(population - lag(population)),
    # chatgpt was missing absolute
    relative_change = (absolute_change / lag(population)) * 100
  )

# Display the resulting data
head(arranged_data)

# (d) Solution
d_data <- arranged_data %>%
  group_by(country) %>% summarize(
    max_absolute_change = max(absolute_change, na.rm = TRUE),
    min_absolute_change = min(absolute_change, na.rm = TRUE),
    max_relative_change = max(relative_change, na.rm = TRUE),
    min_relative_change = min(relative_change, na.rm = TRUE)
  )
d_data

german_data <- tidy_data %>% filter(country == "Germany")
plot(
  german_data$year,
  german_data$population,
  main = "Deutsche Population 1960-2020",
  xlab = "Jahre",
  ylab = "Population"
)
modell <- lm(german_data$population ~ german_data$year, data = german_data)
intercept <- coef(modell)[1]
slope <- coef(modell)[2]

# Plot the scatterplot
plot(german_data$year,
     german_data$population,)
abline(a = intercept, b = slope, col = "red", lwd = 2)
#abline(modell, col = 'red', lwd = 2)
modell
library(dplyr)




#O hate everything about this tool
# Filter the data for Germany
german_data <- tidy_data %>% 
  filter(country == "Germany")

# Plot the scatterplot
plot(
  x = german_data$year,
  y = german_data$population,
  main = "Deutsche Population 1960-2020",
  xlab = "Jahre",
  ylab = "Population"
)

# Fit a linear regression model
modell <- lm(population ~ year, data = german_data)

# Extract coefficients
intercept <- coef(modell)[1]
slope <- coef(modell)[2]

# Add the regression line
abline(modell, col = 'red', lwd = 2)

# (e)
german_data <- tidy_data %>%
  filter(country == "Germany")
head(german_data)

# Load the ggplot2 library
library(ggplot2)

# Assuming your data frame is called 'german_data'

# Convert year to numeric
german_data$year <- as.numeric(german_data$year)

# Create the scatterplot with linear regression line
ggplot(german_data, aes(x = year, y = population)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line
  labs(x = "Year", y = "Population", title = "Population Trend in Germany")

