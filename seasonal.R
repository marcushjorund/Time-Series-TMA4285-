# Data exploration
# Load required packages
library(tidyverse)
library(lubridate)
setwd('/Users/axelhagen/Documents/2024H/TMA4285/')

# Read and prepare electricity data
elData <- read.csv("electricity_production.csv") %>%
  mutate(
    date = ym(måned),  # Convert Swedish/Norwegian month format to date
    year = year(date),
    month = month(date)
  )

# Read and prepare weather data
weatherData <- read.csv("weather.csv") %>%
  mutate(
    date = ymd_hms(referenceTime),
    year = year(date),
    month = month(date)
  )
# Combine datasets
combined_data <- electricity_data %>%
  select(date, year, month, electricity = value) %>%
  inner_join(
    weather_data %>% select(date, year, month, precipitation = value),
    by = c("year", "month")
  ) %>%
  select(date = date.x, year, month, electricity, precipitation)

# Initial visualization of electricity production
ggplot(electricity_data, aes(x = date, y = value)) +
  geom_line() +
  labs(
    title = "Monthly Electricity Production in Trøndelag",
    x = "Date",
    y = "Electricity Production (MWh)"
  ) +
  theme_minimal()

# Initial visualization of precipitation
ggplot(weather_data, aes(x = date, y = value)) +
  geom_line() +
  labs(
    title = "Monthly Precipitation in Trondheim",
    x = "Date",
    y = "Precipitation (mm)"
  ) +
  theme_minimal()

# Calculate basic statistics
electricity_stats <- electricity_data %>%
  summarise(
    mean_production = mean(value, na.rm = TRUE),
    sd_production = sd(value, na.rm = TRUE),
    min_production = min(value, na.rm = TRUE),
    max_production = max(value, na.rm = TRUE)
  )

weather_stats <- weather_data %>%
  summarise(
    mean_precipitation = mean(value, na.rm = TRUE),
    sd_precipitation = sd(value, na.rm = TRUE),
    min_precipitation = min(value, na.rm = TRUE),
    max_precipitation = max(value, na.rm = TRUE)
  )

# Create seasonal plots
electricity_seasonal <- electricity_data %>%
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
