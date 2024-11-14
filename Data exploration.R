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

write.csv(weather_data, file = "cleaned_weather_data.csv")
write.csv(electricity_production_data, file = "cleaned_electricity_production_data.csv")
n <- n_electricity
ggplot(data = NULL, aes(x = seq(n))) +
  geom_line(data = weather_data[1:n,], aes(y = scale(value)), col = "red") +
  geom_line(data = electricity_production_data[1:n,], aes(y = scale(value)), col = "blue") +
  theme_bw() + 
  ylab("Scaled values") +
  xlab("time") +
  ggtitle("Scaled percipitation and electricity production data")
  

#ACF, PACF, and CCF of weather data
percipitation_data <- weather_data$value
electricity_data <- electricity_production_data$value
ccf <- ggCcf(percipitation_data[1:n_electricity], electricity_data, lag.max = 30) +
  ggtitle("")+
  theme_bw()
ggAcf(percipitation_data) +
  theme_bw() +
  ggtitle("ACF of monthly percipition")
ggPacf(percipitation_data) +
  theme_bw() + 
  ggtitle("PACF of montly percipitation")
ggplot(data = NULL,aes(x = seq(length(percipitation_data)), y = percipitation_data)) +
  theme_bw() +
  xlab("time") + 
  geom_line() + 
  ggtitle("Monthy percipitation from Jan. 1993 - Sep. 2019")

#first difference
#differencing the percipitation data is maybe not 
dpercipitation_data <- diff(percipitation_data, lag = 1)
ggplot(data = NULL,aes(x = seq(length(dpercipitation_data)), y = dpercipitation_data)) +
  theme_bw() +
  xlab("time") + 
  geom_line() + 
  ggtitle("First difference monthly percipitation from Jan. 1993 - Sep. 2019")

ggAcf(dpercipitation_data) + 
  theme_bw() + 
  ggtitle("ACF of first differenced montly percipitation")
ggPacf(dpercipitation_data) +
  theme_bw() +
  ggtitle("PACF of first differenced montly percipitation")

d12percipitation_data <- diff(percipitation_data, lag = 12)
ggplot(data = NULL,aes(x = seq(length(d12percipitation_data)), y = d12percipitation_data)) +
  theme_bw() +
  xlab("time") + 
  geom_line() + 
  ggtitle("Seasonal difference monthly percipitation from Jan. 1993 - Sep. 2019")
ggAcf(d12percipitation_data, lag.max = 40) + 
  theme_bw() + 
  ggtitle("ACF of seasonal differenced monthly percipitation")
ggPacf(d12percipitation_data, lag.max = 40) +
  theme_bw() +
  ggtitle("PACF of seasonal differenced monthly percipitation")

ddpercipitation_data <- diff(dpercipitation_data, lag = 12)
ggAcf(ddpercipitation_data) + 
  theme_bw() + 
  ggtitle("ACF of seasonal and first differenced montly percipitation")
ggPacf(ddpercipitation_data) +
  theme_bw() +
  ggtitle("PACF of seasonal and firstdifferenced montly percipitation")


#no obvious advantage of taking a first and seasonal difference
#if modelling percipitation, seasonal differences are adequate

#electricity data
acf <- ggAcf(electricity_data, lag.max = 30) +
  ggtitle("")+
  theme_bw() 
pacf <- ggPacf(electricity_data, lag.max = 30) +
  ggtitle("")+
  theme_bw() 
library(gridExtra)
grid.arrange(ccf, acf, pacf,ncol = 3, top = "ACF, PACF and CCF (with percipitation) of electricity production data")
wdelectricity_data <- diff(electricity_data,lag = 1)
ggplot(data = NULL, aes(x = seq(length(delectricity_data)), y = delectricity_data)) +
  geom_line() +
  theme_bw() + 
  xlab("time") +
  ggtitle("First difference monthly electricity production from Jan. 1993 - Dec. 2011")

ggAcf(delectricity_data) +
  theme_bw() + 
  ggtitle("ACF of first differenced electricity production data")
ggPacf(delectricity_data) +
  theme_bw() +
  ggtitle("PACF of first differenced electricity production data")

d12electricity_data <- diff(electricity_data, lag = 12)
ggplot(data = NULL, aes(x = seq(length(d12electricity_data)), y = d12electricity_data)) +
  geom_line() +
  theme_bw() + 
  xlab("time") +
  ylab("Lag 12 differenced electricity produciton (MWh)")+
  ggtitle("Seasonal difference monthly electricity production from Jan. 1993 - Dec. 2011")

#we still see significant deviations in acf and pacf plots when only first or seasonal
#differencing is performed
#trying with seasonal differencing
ddelectricity_data <- diff(delectricity_data, lag = 12)
ggplot(data = NULL, aes(x = seq(length(ddelectricity_data)), y = ddelectricity_data)) +
  geom_line() +
  theme_bw() + 
  ylab("ddelectricity data") +
  xlab("time")
  ggtitle("Seasonal and first difference monthly electricity production from Jan. 1993 - Dec. 2011")

ggAcf(ddelectricity_data,  lag.max= 50) +
  theme_bw() +
  ggtitle("ACF of seasonal and first differenced electricity production data")
ggPacf(ddelectricity_data, lag.max = 50) +
  theme_bw() + 
  ggtitle("PACF of seasonal and first differenced electricity production data")

#consistent with true SARIMA model


