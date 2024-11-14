#function to create differencing of vectors, allowing for iterated differencing
electricity_production_data <- read.csv(file = "cleaned_electricity_production_data.csv")
weather_data <- read.csv(file = "cleaned_weather_data.csv")
#Regression with autocorrelated errors
#We assume a linear model for the relationship between electricity production and percipitation
n_electricity <- dim(electricity_production_data)[1]
data <- data.frame(y = electricity_production_data$value, x = weather_data$value[1:n_electricity])
model_lm <- lm(y~x, data = data)
electricity_data_res <- data$y-model_lm$fitted.values
model_lm$coef
acf(electricity_data_res)
acf(electricity_production_data$value)
pacf(electricity_data_res)

delectricity_data_res <- diff(electricity_data_res, lag = 1)
plot(delectricity_data_res, type = "l")
acf(delectricity_data_res,lag.max = 40)
pacf(delectricity_data_res, lag.max = 40)

ddelectricity_data_res <- diff(delectricity_data_res, lag = 12)
plot(ddelectricity_data_res, type = "l")
acf(ddelectricity_data_res, lag.max = 40)
pacf(ddelectricity_data_res, lag.max = 40)

#We note that the lag1 + lag12 differencing of the time series y-XB makes it stationary
library(forecast)
model_auto_arima <- auto.arima(electricity_production_data$value, xreg = weather_data$value[1:n_electricity])
model_arima <- arima(electricity_production_data$value,, xreg = weather_data$value[1:n_electricity], order = c(1,1,1), seasonal = list(order = c(0,1,1), period = 12))
plot(model_arima$residuals)
U <- matrix(0, nrow = n_electricity, ncol = 2)
U[1:n_electricity,1] <- rep(1, n_electricity)
U[1:n_electricity,2] <- weather_data$value[1:n_electricity]
plot(electricity_production_data$value - (Gamma%*%t(U))[1],type = "l")
kalman_filter_autocorrelated <- function(parameters,y = electricity_production_data$value,u = U ,n_ahead = 1){
  phi <- parameters[1]
  theta <- parameters[2]
  Theta <- parameters[3]
  R <- Q <-  parameters[4]
  beta_0 <- parameters[5]
  beta_1 <- parameters[6]
  Gamma <- matrix(c(beta_0,beta_1),nrow = 1)
  mu_0 <- parameters[7]*matrix(1, nrow = 14)
  cov_0 <- parameters[8]*diag(14)
  F <- matrix(0, 14,14)
  F[1:13,2:14] <- diag(13)
  F[1:14,1] <- c(1+phi, -phi,0,0,0,0,0,0,0,0,0, 1, -1-phi,phi)
  G <- matrix(c(1+phi+theta, -phi, 0,0,0,0,0,0,0,0,0,1+Theta, -1-phi+theta*Theta,phi),nrow = 14)
  A <- matrix(c(1,rep(0,13)),nrow = 1)
  T <- length(y)
  n <- length(mu_0)
  x_pred <- matrix(0,n,T+1)
  x_filt <- matrix(0,n,T)
  cov_pred <- array(0, dim = c(n,n,T+1))
  cov_filt <- array(0, dim = c(n,n,T))
  innovations <- rep(0,T)
  sigma <- rep(0,T)
  #Initialize
  x_pred[,1] <- F%*%mu_0
  cov_pred[,,1] <- F%*%cov_0%*%t(F) + Q*G%*%t(G)
  x_filt[,1] <- mu_0
  cov_filt[,,1] <- cov_0
  log_lik_kalman <- 0
  for (t in 1:T){
    #Autocorrelated error Kalman filter
    innovations[t] <- y[t] - A%*%x_pred[,t] - Gamma%*%u[t,]
    sigma[t] <- A%*%cov_pred[,,t]%*%t(A)+R
    #Kalman gain
    K <- (F%*%cov_pred[,,t]%*%t(A) + R*G)%*%solve(sigma[t])
    x_pred[,t+1] <- F%*%x_pred[,t]+K*innovations[t]
    cov_pred[,,t+1] <- F%*%cov_pred[,,t]%*%t(F) + Q*G%*%t(G) - sigma[t]*K%*%t(K)
    
    #Filtering
    x_filt[,t] <- x_pred[,t] + cov_pred[,,t]%*%t(A)%*%solve(sigma[t])*innovations[t]
    cov_filt[,,t] <- cov_pred[,,t] - cov_pred[,,t]%*%t(A)%*%solve(sigma[t])%*%A%*%cov_pred[,,t]
    log_lik_kalman <- log_lik_kalman + 0.5*(log(abs(sigma[t]))+innovations[t]^2/sigma[t])
  }
  if(n_ahead != 0){
    x_fore <- matrix(0, n, n_ahead+1)
    x_fore[,1] <- x_pred[,T+1]
    cov_fore <- array(0, dim = c(n,n,n_ahead+1))
    cov_fore[,,1] <- cov_pred[,,T+1]
    
    for (t in 1:n_ahead){
      x_fore[,t+1] <- F%*%x_fore[,t]
      cov_fore[,,t+1] <- F%*%cov_fore[,,t]%*%t(F) + Q*G%*%t(G)
    }
    return(list(innovations = innovations, sigma = sigma, x_pred = x_pred, x_filt = x_filt, cov_pred = cov_pred,
                cov_filt = cov_filt, x_fore = x_fore, cov_fore = cov_fore, log_lik_kalman = log_lik_kalman))
  }
  return(list(innovations = innovations, sigma = sigma, x_pred = x_pred, x_filt = x_filt, cov_pred = cov_pred,
              cov_filt = cov_filt, log_lik_kalman = log_lik_kalman))
}


phi <- 1
theta <- 1
Theta <- 1
R <- Q <-  1
beta_0 <-model_lm$coefficients[1]
beta_1 <- model_lm$coefficients[2]
mu_0 <- mean(electricity_production_data$value)
cov_0 <- var(electricity_production_data$value)
parameters = c(phi, theta, Theta, R, beta_0, beta_1, mu_0, cov_0)
names(parameters) <- c("phi", "theta", "Theta", "R", "beta0", "beta1","mu_0", "cov_0")
kalman <- kalman_filter_autocorrelated(parameters,y = electricity_production_data$value,u = U)
plot(kalman$sigma,type = "l")
plot(kalman$innovations/sqrt(kalman$sigma),type = "l",col="blue")


log_lik_kalman <- function(parameters){kalman_filter_autocorrelated(parameters, n_ahead = 0)$log_lik_kalman}
maximum_likelihood_kalman<- function(y,u,parameters){
  opt <- optim(par = parameters, fn = log_lik_kalman, method = "BFGS")
}
ml_kalman <- maximum_likelihood_kalman(y = electricity_production_data$value, u = U, parameters = parameters)
ml_kalman$par
optimal_x_pred <- kalman_filter_autocorrelated(parameters = ml_kalman$par)

plot(optimal_x_pred$innovations,type = "l")
plot(kalman$x_pred[1,]+ (Gamma%*%t(U))[1,],type = "l", col = "blue")
lines(electricity_production_data$value, col = "red")
plot(optimal_x_pred$x_filt[1,]+(Gamma%*%t(U))[1,],type = "l", col = "blue")
lines(electricity_production_data$value, col = "red")


plot(optimal_x_pred$sigma, type = "l", col = "pink")
optimal_x_pred$sigma
#Maybe use later?
kalman_filter_uncorrelated <- function(y, F, G, A, Gamma,u,mu_0, cov_0,R,Q, n_ahead = 1){
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
    return(list(innovations = innovations, sigma = sigma, innovations_fore = innovations_fore,
                sigma_fore = sigma_fore, x_pred = x_pred, x_filt = x_filt, x_fore_pred = x_fore_pred,
                x_fore_filt = x_fore_filt, cov_pred = cov_pred, cov_filt = cov_filt, cov_fore_pred = cov_fore_pred,
                cov_fore_filt = cov_fore_filt))
  }
  return(list(innovations = innovations, sigma = sigma, x_pred = x_pred, x_filt = x_filt, 
              cov_pred = cov_pred, cov_filt = cov_filt))
}