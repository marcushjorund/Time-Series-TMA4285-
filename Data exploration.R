library("GGally")
weather_data_org <- read.csv(file = "weather_data.csv")
weather_data <- weather_data_org
n_weather_org <- dim(weather_data)[1]
ggplot(data = weather_data, aes(x = seq(n_weather_org), y = value)) +
  geom_line() +
  theme_bw() +
  xlab("time") + 
  ggtitle("Monthy percipitation from dec. 1953 - sep. 2019")
head(weather_data, n = 20L)

weather_data[c("year", "month", "date_time")] <- do.call(rbind, strsplit(weather_data$referenceTime, "-"))
weather_data <- subset(weather_data, select = -c(referenceTime, date_time))
weather_data <- weather_data[(weather_data$year > electricity_production_data$year[1]),]
row.names(weather_data) <- NULL
n_weather <- dim(weather_data)[1]
n_weather
head(weather_data)

electricity_production_data = read.csv(file = "electricity_production_data.csv")
n_electricity = dim(electricity_production_data)[1]
dim(electricity_production_data)[1]
ggplot(data = electricity_production_data, aes(x = seq(n_electricity), y = value)) +
  geom_line() +
  theme_bw() + 
  xlab("time") +
  ggtitle("Monthly electricity production from 1993 - 2011")

electricity_production_data[c("year", "month")] <- do.call(rbind, strsplit(electricity_production_data$måned, "M"))
electricity_production_data <- subset(electricity_production_data, select = -c(måned))

ggplot(data = NULL, aes(x = seq(n_electricity))) +
  geom_line(data = weather_data[1:n_electricity,], aes(y = scale(value)), col = "red") +
  geom_line(data = electricity_production_data, aes(y = scale(value)), col = "blue") +
  theme_bw() + 
  xlab("time") 


