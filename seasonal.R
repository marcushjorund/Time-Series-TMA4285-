# Data exploration
# Load required packages
library(tidyverse)
library(lubridate)

# Read and prepare electricity data
elData <- read.csv("electricity_production_data.csv") %>%
  mutate(
    date = ym(måned),  # Convert Swedish/Norwegian month format to date
    year = year(date),
    month = month(date)
  )

# Read and prepare weather data
weatherData <- read.csv("weatherData.csv") %>%
  mutate(
    date = ymd_hms(referenceTime),
    year = year(date),
    month = month(date)
  )
# Combine datasets
combined_data <- elData %>%
  select(date, year, month, electricity = value) %>%
  inner_join(
    weatherData %>% select(date, year, month, precipitation = value),
    by = c("year", "month")
  ) %>%
  select(date = date.x, year, month, electricity, precipitation)

# Initial visualization of electricity production
ggplot(elData, aes(x = date, y = value)) +
  geom_line() +
  labs(
    title = "Monthly Electricity Production in Trøndelag",
    x = "Date",
    y = "Electricity Production (MWh)"
  ) +
  theme_minimal()

# Initial visualization of precipitation
ggplot(weatherData, aes(x = date, y = value)) +
  geom_line() +
  labs(
    title = "Monthly Precipitation in Trondheim",
    x = "Date",
    y = "Precipitation (mm)"
  ) +
  theme_minimal()

# Calculate basic statistics
electricity_stats <- elData %>%
  summarise(
    mean_production = mean(value, na.rm = TRUE),
    sd_production = sd(value, na.rm = TRUE),
    min_production = min(value, na.rm = TRUE),
    max_production = max(value, na.rm = TRUE)
  )

weather_stats <- weatherData %>%
  summarise(
    mean_precipitation = mean(value, na.rm = TRUE),
    sd_precipitation = sd(value, na.rm = TRUE),
    min_precipitation = min(value, na.rm = TRUE),
    max_precipitation = max(value, na.rm = TRUE)
  )

# Create seasonal plots
electricity_seasonal <- elData %>%
  ggplot(aes(x = month, y = value, group = year)) +
  geom_line(alpha = 0.3) +
  stat_summary(aes(group = 1), fun = mean, color = "red", size = 1) +
  labs(
    title = "Seasonal Plot of Electricity Production",
    x = "Month",
    y = "Electricity Production (MWh)"
  ) +
  theme_minimal()

# Check for correlation between precipitation and electricity production
correlation <- cor(combined_data$electricity, combined_data$precipitation)

# Create scatter plot of precipitation vs electricity production
scatter_plot <- ggplot(combined_data, aes(x = precipitation, y = electricity)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relationship between Precipitation and Electricity Production",
    x = "Precipitation (mm)",
    y = "Electricity Production (MWh)"
  ) +
  theme_minimal()
