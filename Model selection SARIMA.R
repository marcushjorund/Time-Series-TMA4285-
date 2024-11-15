#function to create differencing of vectors, allowing for iterated differencing
library(ggplot2)
library(gridExtra)
electricity_production_data <- read.csv(file = "cleaned_electricity_production_data.csv")
weather_data <- read.csv(file = "cleaned_weather_data.csv")
#Regression with autocorrelated errors
#We assume a linear model for the relationship between electricity production and percipitation
n_electricity <- dim(electricity_production_data)[1]
n_electricity <- 100
data <- data.frame(y = electricity_production_data$value[1:n_electricity], x = weather_data$value[1:n_electricity])
data_lm <- data.frame(y = electricity_production_data$value[1:228], x = weather_data$value[1:228])
model_lm <- lm(y~x, data = data_lm)
electricity_data_res <- data_lm$y-model_lm$fitted.values
detrended <- ggplot(data = NULL, aes(x = seq(1,228), y = electricity_data_res)) +
  theme_bw() +
  xlab("time") +
  geom_line() +
  ylab("d = 0, D = 0, s = 0")

model_lm$coef
acf1 <- acf(electricity_data_res)
pacf1 <- pacf(electricity_data_res)

delectricity_data_res <- diff(electricity_data_res, lag = 1)
plot(delectricity_data_res, type = "l")
acf(delectricity_data_res,lag.max = 40)
pacf(delectricity_data_res, lag.max = 40)

ddelectricity_data_res <- diff(delectricity_data_res, lag = 12)

dddetrended <- ggplot(data = NULL, aes(x = seq(1,215), y = ddelectricity_data_res)) +
  theme_bw() +
  xlab("time") +
  geom_line() +
  ylab("d = 1, D = 1, s = 0")
grid.arrange(detrended, dddetrended, top ="Detrended and detrended with first and seasonal difference") 
plot(ddelectricity_data_res, type = "l")
acfdd <- ggAcf(ddelectricity_data_res, lag.max = 30) +
  theme_bw() + 
  ggtitle("")
pacfdd <- ggPacf(ddelectricity_data_res, lag.max = 30) +
  theme_bw() +
  ggtitle("")
grid.arrange(acfdd, pacfdd, ncol = 2, top = "ACF and PACF of the detrended differenced series")
#We note that the lag1 + lag12 differencing of the time series y-XB makes it stationary

U <- matrix(0, nrow = n_electricity, ncol = 2)
U[1:n_electricity,1] <- rep(1, n_electricity)
U[1:n_electricity,2] <- weather_data$value[1:n_electricity]

kalman_filter_autocorrelated <- function(parameters,y = data$y,u = U ,n_ahead = 1){
  T <- length(y)
  n <- 14
  phi <- parameters[1]
  theta <- parameters[2]
  Theta <- parameters[3]
  R <- Q <-  parameters[4]
  beta_0 <- parameters[5]
  beta_1 <- parameters[6]
  Gamma <- matrix(c(beta_0,beta_1),nrow = 1)
  mu_0 <- parameters[7]*matrix(1,nrow = 14)
  cov_0 <- parameters[8]*diag(14)
  F <- matrix(0, 14,14)
  F[1:13,2:14] <- diag(13)
  F[1:14,1] <- c(1+phi, -phi,0,0,0,0,0,0,0,0,0, 1, -1-phi,phi)
  G <- matrix(c(1+phi+theta, -phi, 0,0,0,0,0,0,0,0,0,1+Theta, -1-phi+theta*Theta,phi),nrow = 14)
  A <- matrix(c(1,rep(0,13)),nrow = 1)
  x_pred <- matrix(0,n,T+1)
  x_filt <- matrix(0,n,T)
  cov_pred <- array(0, dim = c(n,n,T+1))
  cov_filt <- array(0, dim = c(n,n,T))
  innovations <- rep(0,T)
  sigma <- rep(0,T)
  K <- matrix(0, n,T)
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
    K[,t] <- (F%*%cov_pred[,,t]%*%t(A) + R*G)%*%solve(sigma[t])
    
    x_pred[,t+1] <- F%*%x_pred[,t]+K[,t]*innovations[t]
    cov_pred[,,t+1] <- F%*%cov_pred[,,t]%*%t(F) + Q*G%*%t(G) - sigma[t]*K[,t]%*%t(K[,t])
    
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
    return(list(innovations = innovations, sigma = sigma, K=K, x_pred = x_pred, x_filt = x_filt, cov_pred = cov_pred,
                cov_filt = cov_filt, x_fore = x_fore, cov_fore = cov_fore, log_lik_kalman = log_lik_kalman))
  }
  return(list(innovations = innovations, sigma = sigma, K = K, x_pred = x_pred, x_filt = x_filt, cov_pred = cov_pred,
              cov_filt = cov_filt, log_lik_kalman = log_lik_kalman))
}

phi <- 1
theta <- 1
Theta <- 1
R <- Q <-  1
beta_0 <-model_lm$coefficients[1]
beta_1 <- model_lm$coefficients[2]
mu_0 <- 0
cov_0 <- var(data$y)
parameters = c(phi, theta, Theta, R, beta_0, beta_1,mu_0,cov_0)
names(parameters) <- c("phi", "theta", "Theta", "R", "beta0", "beta1","mu0","cov0")
kalman <- kalman_filter_autocorrelated(parameters,y = data$y,u = U)
plot(kalman$x_filt[1,] + (Gamma%*%t(U))[1,],type = "l")
lines(data$y,col = "magenta")
plot(kalman$innovations/sqrt(kalman$sigma),type = "l")

log_lik_kalman <- function(parameters){kalman_filter_autocorrelated(parameters, n_ahead = 0)$log_lik_kalman}
maximum_likelihood_kalman<- function(y,u,parameters){
  opt <- optim(par = parameters, fn = log_lik_kalman, method = "BFGS")
}
ml_kalman <- maximum_likelihood_kalman(y = data$y, u = U, parameters = parameters)
ml_kalman$par
Gamma <- matrix(c(model_lm$coefficients), nrow = 1)
optimal_x_pred <- kalman_filter_autocorrelated(parameters = ml_kalman$par)
#Innovations and variance-covariance matrix
plot(optimal_x_pred$innovations,type = "l")
plot(optimal_x_pred$sigma[16:100], type = "l")
plot(optimal_x_pred$sigma[20:100],type = "l")
#Predictions

x_pred <- ggplot(data = NULL, aes(x = 1:n_electricity)) +
  theme_bw() +
  geom_line(data = NULL, aes(y = optimal_x_pred$x_pred[1,1:n_electricity]+ (Gamma%*%t(U))[1,],col = "Predicted")) +
  geom_line(data = NULL, aes(y = data$y, col = "Actual")) +
  scale_color_manual(name = "Predicted versus actual values", values = c("Predicted" = "red", "Actual" = "blue")) +
  ggtitle("Predicted versus actual electricity production") +
  xlab("time")+
  ylab("Predicted Y (MWh)")
x_filt <- ggplot(data = NULL, aes(x = 1:n_electricity)) +
  theme_bw() +
  geom_line(data = NULL, aes(y = optimal_x_pred$x_filt[1,1:n_electricity]+ (Gamma%*%t(U))[1,],col = "Filtered")) +
  geom_line(data = NULL, aes(y = data$y, col = "Actual")) +
  scale_color_manual(name = "Filtered versus actual values", values = c("Filtered" = "red", "Actual" = "blue")) +
  ggtitle("Predicted versus actual electricity production") +
  xlab("time")+
  ylab("Predicted Y (MWh)")
grid.arrange(x_pred,x_filt, nrow = 2)
plot(optimal_x_pred$x_pred[1,]+ (Gamma%*%t(U))[1,],type = "l", col = "blue")
lines(electricity_production_data$value, col = "red")
#Filtering working
plot(optimal_x_pred$x_filt[1,]+(Gamma%*%t(U))[1,],type = "l")
lines(data$y, col = "red")

#forecasting
optimal_x_fore <- kalman_filter_autocorrelated(parameters <- ml_kalman$par,n_ahead = 10)
plot(optimal_x_fore$x_fore[1,],type = "l")

