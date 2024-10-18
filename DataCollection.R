############################################################################################
############################### Script for data gathering ##################################
############################################################################################

# Libraries and functions ----

# Libraries needed (jsonlite and tidyr must be installed in R)
library(tidyverse) # Plotting and data wrangling
library(stringr) # String manipulation
library(jsonlite) # API help
library(PxWebApiData) # For the SSB API

# We fetch the weather data of interest ----

# Helper function for fetching weather data:
get.weather.data.from.specifications <- function(client_id, sources, elements, referenceTime){
  endpoint <- paste0("https://", client_id, "@frost.met.no/observations/v0.jsonld")

  # Build the URL to Frost
  url <- paste0(
    endpoint, "?",
    "sources=", sources,
    "&referencetime=", referenceTime,
    "&elements=", elements
  )
  # Issue an HTTP GET request and extract JSON data
  xs <- try(fromJSON(URLencode(url),flatten=T))

  # Check if the request worked, print out any errors
  if (class(xs) != 'try-error') {
    print("Data retrieved from frost.met.no!")
    invisible(xs)
  } else {
    print("Error: the data retrieval was not successful!")
  }
}

# Insert your own client ID here, for user creation, see: https://frost.met.no/authentication.html
client_id <- '...'

# Locations for observations
sources <- 'SN69100' # This is a station in Trondheim, which we use as a proxy for weather in the overall region
# To see a full list of available stations, go to https://frost.met.no/sources/v0.jsonld

# Types of measurements we look at, for a full list of available elements, see https://frost.met.no/elementtable
elements <- 'sum(precipitation_amount P1M)'

referenceTime <- '1951-01-01/2019-12-12'

weatherData <- get.weather.data.from.specifications(client_id = client_id, sources = sources,
                                  elements = elements, referenceTime = referenceTime)

weatherData <- unnest(data = weatherData$data, cols = observations) |>
  filter(timeOffset == "PT18H") |>
  select(referenceTime, value)

weatherData$referenceTime <- as.POSIXct(x = weatherData$referenceTime)

# We write the data to a csv file:
write_csv(x = weatherData, file = "weather_data.csv")

# R has a data structure for time series, which we use, here for the
rainData_ts <- ts(data = weatherData$value)

# We fetch the electricity data of interest ----

# Data from ssb, link here to more tables: https://www.ssb.no/statbank/list/elektrisitet

elData <- ApiData1("https://data.ssb.no/api/v0/no/table/06903/",
                      Region = c("16", "17"), Tid = c(1:228))

elData <- elData |>
  group_by(måned) |>
  summarise(value = sum(value))

# We write the data to a csv file:
write_csv(x = elData, file = "electricity_production_data.csv")

# Since we have less data on electricity production, we need to make sure the time indices match up!
elData_ts <- ts(data = elData$value, start = which(weatherData$referenceTime==as.POSIXct(x = "1993-01-01")))

# We check the acf, pacf and ccf ----
plot(rainData_ts)
plot(elData_ts)

acf(elData_ts)
pacf(elData_ts)
acf(rainData_ts)
pacf(rainData_ts)
ccf(x = rainData_ts, y = elData_ts)




