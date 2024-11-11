#function to create differencing of vectors, allowing for iterated differencing
electricity_production_data <- read.csv(file = "cleaned_electricity_production_data.csv")
weather_data <- read.csv(file = "cleaned_weather_data.csv")
#Regression with autocorrelated errors
#We assume a linear model for the relationship between electricity production and percipitation
n_electricity <- dim(electricity_production_data)[1]
data <- data.frame(y = electricity_production_data$value, x = weather_data$value[1:n_electricity])
model_lm <- lm(y~x, data = data)
electricity_data_res <- data$y-model$fitted.values
model_lm$coef
acf(electricity_data_res)
acf(electricity_production_data$value)
pacf(electricity_data_res)

delectricity_data_res <- diff(electricity_data_res, lag = 12)
plot(delectricity_data_res, type = "l")
acf(delectricity_data_res,lag.max = 40)
pacf(delectricity_data_res, lag.max = 40)

ddelectricity_data_res <- diff(delectricity_data_res, lag = 12)
plot(ddelectricity_data_res, type = "l")
acf(ddelectricity_data_res, lag.max = 40)
pacf(ddelectricity_data_res, lag.max = 40)

#We note that the lag1 + lag12 differencing of the time series y-XB makes it stationary
library(forecast)
c <- array(seq(1,2*3*4),dim = c(2,3,4))

model_auto_arima <- auto.arima(electricity_production_data$value, xreg = weather_data$value[1:n_electricity])
model_arima <- arima(electricity_production_data$value,, xreg = weather_data$value[1:n_electricity], order = c(1,1,1), seasonal = list(order = c(0,1,1), period = 12))
model_arima$aic
kalman_filter_autocorrelated <- function(x,y, F, G, A, Gamma,u,mu_0, cov_0,R,Q, n_ahead = 1){
  T <- length(y)
  n <- length(mu_0)
  x_pred <- matrix(0,n,T+1)
  x_filt <- matrix(0,n,T)
  cov_pred <- array(0, dim = c(n,n,T+1))
  cov_filt <- array(0, dim = c(n,n,T))
  innovations <- rep(0,T)
  sigma <- array(0, dim = c(n,n,T))
  #Initialize
  x_pred[,1] <- F%*%mu_0
  cov_pred[,,1] <- F%*%cov_0%*%t(F) + G%*%Q%*%t(G)
  x_filt[,1] <- mu_0
  cov_filt[,,1] <- cov_0
  for (i in 1:T){
    #Autocorrelated error Kalman filter
    innovations[t] <- y[t] - A%*%x_pred[,t] - Gamma%*%u[t]
    sigma[,,t] <- solve(A%*%cov_pred[,,t]%*%t(A)+R)
    #Kalman gain
    K <- (F%*%cov_pred[,,t]%*%t(A) + G%*%S)%*%solve(sigma[,,t])
    x_pred[,t+1] <- F%*%x_pred[,t]+K*innovations[t]
    cov_pred[,,t+1] <- F%*%cov_pred[,,t]%*%t(F) + G%*%Q%*%t(G) + K%*%sigma[,,t]%*%t(K)
    
    #Filtering
    x_filt[,t] <- x_pred[,t] + cov_pred[,,t]%*%t(A)%*%solve(sigma[,,t])%*%innovations[t]
    cov_filt[,,t] <- cov_pred[,,t] - cov_pred[,,t]%*%t(A)%*%solve(sigma[,,t])%*%A%*%cov_pred[,,t]
  }
  if(n_ahead != 0){
    x_fore <- matrix(0, n, n_ahead)
    cov_fore <- array(0, dim = c(n,n,n_ahead))
    for (t in (T+1):(T+1+n_ahead)){
      K <- (F%*%co)
      x_pred[,t+1] <- F%*%x_pred[,t]
    }
  }
}

kalman_filter_uncorrelated <- function(x,y, F, G, A, Gamma,u,mu_0, cov_0,R,Q, n_ahead = 1){
  T <- length(y)
  n <- length(mu_0)
  x_pred <- matrix(0,n,T)
  x_filt <- matrix(0,n,T)
  cov_pred <- array(0, dim = c(n,n,T))
  cov_filt <- array(0, dim = c(n,n,T))
  innovations <- rep(0,T)
  sigma <- array(0, dim = c(n,n,T))
  #Initialize
  x_filt[,1] <- mu_0
  cov_filt[,,1] <- cov_0
  for (i in 1:T){
    #Autocorrelated error Kalman filter
    #Kalman gain
    x_pred[,t] <- F%*%x_filt[,t]
    cov_pred[,,t] <- F%*%cov_pred[,,t]%*%t(F) + Q
    innovations[t] <- y[t] - A%*%x_pred[,t] - Gamma%*%u[t]
    sigma[,,t] <- solve(A%*%cov_[,,t]%*%t(A)+R)
    K <- (F%*%cov_pred[,,t]%*%t(A))%*%solve(sigma[,,t])
    
    #Filtering
    x_filt[,t] <- x_pred[,t] + K%*%innovations[t]
    cov_filt[,,t] <- cov_pred[,,t]%*%t(A)%*%solve(sigma[,,t])
  }
  if(n_ahead != 0){
    x_fore_pred <- matrix(0, n, n_ahead)
    x_fore_filt <- matrix(0,n,n_ahead)
    cov_fore_pred <- array(0, dim = c(n,n,n_ahead))
    cov_fore_filt <- array(0, dim = c(n,n,n_ahead))
    x_fore_filt[,1] <- x_filt[,T]
    cov_fore_filt[,,1] <- cov_filt[,,T]
    innovations_fore <- rep(0,n_ahead)
    sigma_fore <- array(0, dim = c(n,n,n_ahead))
    for (t in i:n_ahead){
      x_fore_pred[,t] <- F%*%x_fore_filt[,t]
      cov_fore_pred[,,t] <- F%*%cov_fore_pred[,,t]%*%t(F) + Q
      innovations[t] <- y[t] - A%*%x_pred[,t] - Gamma%*%u[t]
      sigma[,,t] <- solve(A%*%cov_[,,t]%*%t(A)+R)
      K <- (F%*%cov_pred[,,t]%*%t(A))%*%solve(sigma[,,t])
      
      #Filtering
      x_filt[,t] <- x_pred[,t] + K%*%innovations[t]
      cov_filt[,,t] <- cov_pred[,,t]%*%t(A)%*%solve(sigma[,,t])
    }
  }
}
