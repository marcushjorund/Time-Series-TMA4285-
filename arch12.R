arch_12 <- function(returns, n_iter = 1000, tol = 1e-6) {
  n <- length(returns)
  returns_sq <- returns^2
  # Use exponential parameterization for numerical stability
  arch_loglik <- function(pars) {
    # Transform parameters back to original scale
    omega <- exp(pars[1])
    alphas <- exp(pars[2:13])/(1 + sum(exp(pars[2:13]))) * 0.9  # Ensure sum < 1
    sigma2 <- numeric(n)
    sigma2[1:12] <- var(returns)
    for (t in 13:n) {
      sigma2[t] <- omega
      for (i in 1:12) {
        sigma2[t] <- sigma2[t] + alphas[i] * returns_sq[t-i]
      }
      sigma2[t] <- max(sigma2[t], 1e-6)  # Prevent negative variance
    }
    ll <- -sum(log(sigma2[13:n]) + returns_sq[13:n]/sigma2[13:n])
    if (!is.finite(ll)) return(.Machine$double.xmax)
    return(-ll)
  }
  # Initial values on log scale
  init_params <- c(log(var(returns) * 0.1), rep(-3, 12))
  # Optimize with more stable method
  opt <- optim(
    par = init_params,
    fn = arch_loglik,
    method = "Nelder-Mead",
    control = list(maxit = n_iter, reltol = 1e-8)
  )
  # Transform parameters back
  params <- c(exp(opt$par[1]),
              exp(opt$par[2:13])/(1 + sum(exp(opt$par[2:13]))) * 0.9)
  names(params) <- c("omega", paste0("alpha", 1:12))
  # Calculate final variances
  sigma2 <- numeric(n)
  sigma2[1:12] <- var(returns)
  for (t in 13:n) {
    sigma2[t] <- params[1]
    for (i in 1:12) {
      sigma2[t] <- sigma2[t] + params[i+1] * returns_sq[t-i]
    }
  }
  list(
    parameters = params,
    conditional_variance = sigma2,
    convergence = opt$convergence,
    log_likelihood = -opt$value
  )
}

prepare_data <- function(data) {
  # Calculate log returns
  log_returns <- diff(log(data$value))
  # Center the returns by subtracting the mean
  centered_returns <- log_returns - mean(log_returns)
  # Remove any infinite or NA values
  centered_returns <- centered_returns[is.finite(centered_returns)]
  return(centered_returns)
}
plot_garch_diagnostics <- function(garch_fit, returns, original_data) {
  # 1. Original time series and volatility
  dates <- original_data$date[-1]  # Remove first date as we used returns
  
  # Create data frame for time series plot
  ts_data <- data.frame(
    Date = dates,
    Returns = returns,
    Volatility = sqrt(garch_fit$conditional_variance)
  )
  
  # Time series plot
  p1 <- ggplot(ts_data, aes(x = Date)) +
    geom_line(aes(y = Returns), color = "darkgrey") +
    geom_line(aes(y = Volatility), color = "blue", alpha = 0.7) +
    theme_minimal() +
    labs(title = "Returns and Estimated Volatility",
         y = "Value",
         x = "Time") +
    theme(legend.position = "bottom")
  
  # 2. QQ plot of standardized residuals
  std_resid <- returns / sqrt(garch_fit$conditional_variance)
  qq_data <- data.frame(
    theoretical = qnorm(ppoints(length(std_resid))),
    sample = sort(std_resid)
  )
  
  p2 <- ggplot(qq_data, aes(x = theoretical, y = sample)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    theme_minimal() +
    labs(title = "Q-Q Plot of Standardized Residuals",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles")
  
  # 3. ACF of squared standardized residuals
  acf_data <- acf(std_resid^2, plot = FALSE)
  acf_df <- data.frame(
    lag = acf_data$lag,
    acf = acf_data$acf
  )
  
  p3 <- ggplot(acf_df, aes(x = lag, y = acf)) +
    geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
    geom_hline(yintercept = c(-1.96/sqrt(length(returns)), 1.96/sqrt(length(returns))),
               linetype = "dashed", color = "red") +
    theme_minimal() +
    labs(title = "ACF of Squared Standardized Residuals",
         x = "Lag",
         y = "ACF")
  
  # 4. Volatility clustering plot
  clust_data <- data.frame(
    Returns = returns,
    Volatility = sqrt(garch_fit$conditional_variance)
  )
  
  p4 <- ggplot(clust_data, aes(x = Returns, y = dplyr::lag(Returns))) +
    geom_point(alpha = 0.5, aes(color = Volatility)) +
    scale_color_viridis_c() +
    theme_minimal() +
    labs(title = "Volatility Clustering",
         x = "Returns(t)",
         y = "Returns(t-1)",
         color = "Volatility")
  
  # Arrange plots in a grid
  grid.arrange(p1, p2, p3, p4, ncol = 2)
  
  # Return diagnostic statistics
  list(
    # Basic statistics of standardized residuals
    mean_std_resid = mean(std_resid),
    sd_std_resid = sd(std_resid),
    skewness = mean((std_resid - mean(std_resid))^3) / sd(std_resid)^3,
    kurtosis = mean((std_resid - mean(std_resid))^4) / sd(std_resid)^4,
    
    # Ljung-Box test for autocorrelation
    ljung_box = Box.test(std_resid, type = "Ljung-Box")$p.value,
    
    # ARCH LM test (simplified version)
    arch_effect = summary(lm(std_resid^2 ~ lag(std_resid^2)))$r.squared
  )
}
# Read and prepare electricity data
elData <- read.csv("electricity_production_data.csv") %>%
  mutate(
    date = ym(måned),  # Convert Swedish/Norwegian month format to date
    year = year(date),
    month = month(date)
  )

# Read and prepare weather data
weatherData <- read.csv("weather_data.csv") %>%
  mutate(
    date = ymd_hms(referenceTime),
    year = year(date),
    month = month(date)
  )
electricity_returns <- prepare_data(elData)
arch_fit <- arch_12(electricity_returns)
plot_garch_diagnostics(arch_fit, electricity_returns, elData)
