library("GGally")
weather_data_org <- read.csv(file = "weather_data.csv")
weather_data <- weather_data_org
n_weather_org <- dim(weather_data_org)[1]
ggplot(data = weather_data, aes(x = seq(n_weather_org), y = value)) +
  geom_line() +
  theme_bw() +
  xlab("time") + 
  ggtitle("Monthy percipitation from Dec. 1950 - Sep. 2019")

electricity_production_data = read.csv(file = "electricity_production_data.csv")
n_electricity = dim(electricity_production_data)[1]
tail(electricity_production_data)
ggplot(data = electricity_production_data, aes(x = seq(n_electricity), y = value)) +
  geom_line() +
  theme_bw() + 
  xlab("time") +
  ggtitle("Monthly electricity production from Jan. 1993 - Dec. 2011")

electricity_production_data[c("year", "month")] <- do.call(rbind, strsplit(electricity_production_data$måned, "M"))
electricity_production_data <- subset(electricity_production_data, select = -c(måned))

weather_data[c("year", "month", "date_time")] <- do.call(rbind, strsplit(weather_data$referenceTime, "-"))
weather_data <- subset(weather_data, select = -c(referenceTime, date_time))
weather_data <- weather_data[(weather_data$year > electricity_production_data$year[1]),]
dim(weather_data)
dim(electricity_production_data)

ggplot(data = NULL, aes(x = seq(n_electricity))) +
  geom_line(data = weather_data[1:n_electricity,], aes(y = scale(value)), col = "red") +
  geom_line(data = electricity_production_data, aes(y = scale(value)), col = "blue") +
  theme_bw() + 
  xlab("time") 

#ACF, PACF, and CCF of data
percipitation_data <- weather_data$value
electricity_data <- electricity_production_data$value
ccf(percipitation_data[1:n_electricity], electricity_data)
acf(percipitation_data)
pacf(percipitation_data)
ggplot(data = NULL,aes(x = seq(length(percipitation_data)), y = percipitation_data)) +
  theme_bw() +
  xlab("time") + 
  geom_line() + 
  ggtitle("Monthy percipitation from Jan. 1993 - Sep. 2019")

#first difference
dpercipitation_data <- diff(percipitation_data, lag = 1)
ggplot(data = NULL,aes(x = seq(length(dpercipitation_data)), y = dpercipitation_data)) +
  theme_bw() +
  xlab("time") + 
  geom_line() + 
  ggtitle("First difference monthly percipitation from Jan. 1993 - Sep. 2019")

acf(dpercipitation_data)
pacf(dpercipitation_data)

#seasonal difference 
ddpercipitation_data <- diff(dpercipitation_data, lag = 12)
ggplot(data = NULL,aes(x = seq(length(ddpercipitation_data)), y = ddpercipitation_data)) +
  theme_bw() +
  xlab("time") + 
  geom_line() + 
  ggtitle("Seasonal and first difference monthly percipitation from Jan. 1993 - Sep. 2019")

acf(ddpercipitation_data)
pacf(ddpercipitation_data)

#no obvious advantage of taking a seasonal difference

#electricity data
dlogelectricity_data <- diff(log(electricity_data),lag = 1)
ggplot(data = NULL, aes(x = seq(length(dlogelectricity_data)), y = dlogelectricity_data)) +
  geom_line() +
  theme_bw() + 
  xlab("time") +
  ggtitle("First difference monthly electricity production from Jan. 1993 - Dec. 2011")

acf(dlogelectricity_data)
pacf(dlogelectricity_data)
#we still see significant deviations in acf and pacf plots
#trying with seasonal differencing
ddlogelectricity_data <- diff(dlogelectricity_data, lag = 12)
ggplot(data = NULL, aes(x = seq(length(ddlogelectricity_data)), y = ddlogelectricity_data)) +
  geom_line() +
  theme_bw() + 
  xlab("time") +
  ggtitle("Seasonal and first difference monthly electricity production from Jan. 1993 - Dec. 2011")

acf(ddlogelectricity_data)
pacf(ddlogelectricity_data)
#consistent with true SARIMA model

