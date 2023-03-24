


stderror <- function(x) sd(x)/sqrt(length(x))

BCa <- function(bootstrap_estimates, bootstrap_sderrors, T_hat, start_sample, sample_sderror, T_estimator, alpha) {
  B <- length(bootstrap_estimates)
  N <- length(start_sample)
  number <- 0
  Ts <- rep(0, N)
  
  for(i in 1:B){
    if (bootstrap_estimates[i] < T_hat){
      number <- number + 1
    }
  }
  
  if (number == 0)
    z0 <- -10
  else if (number == B)
    z0 <- 10
  else
    z0 <- qnorm(number/B)
  
  for(i in 1:N)
  {
    x_i <- start_sample[-i]
    Ts[i] <- T_estimator(x_i)
  }
  
  T_mean <- mean(Ts)
  
  teller <- (T_mean - Ts)^3
  noemer <- (T_mean - Ts)^2
  
  a <- sum(teller)/(6*sum(noemer)^(3/2))
  
  alpha_1 <- pnorm(z0 + (z0 + qnorm(alpha/2))/(1 - a*(z0 + qnorm(alpha/2))))
  alpha_2 <- pnorm(z0 + (z0 + qnorm(1-alpha/2))/(1 - a*(z0 + qnorm(1-alpha/2))))
  
  bootstrap_estimates <- sort(bootstrap_estimates)
  
  T_lo <- bootstrap_estimates[ceiling(B * alpha_1)]
  T_up <- bootstrap_estimates[floor(B * alpha_2)]
  
  CI <- c(T_lo, T_up)
  
  return(CI)
}

Bootstrap_t <- function(bootstrap_estimates, bootstrap_sderrors, T_hat, sample_sderror, alpha) {
  B <- length(bootstrap_estimates)
  Z <- rep(0, B)
  
  for(i in 1:B)
    Z[i] <- (bootstrap_estimates[i]-T_hat)/bootstrap_sderrors[i]
  
  Z <- sort(Z)
  
  h1 <- ceiling(B * alpha/2)
  h2 <- floor(B * (1-alpha/2))
  
  t1 <- Z[h1]
  t2 <- Z[h2]
  
  T_lo <- T_hat-t2*sample_sderror
  T_up <- T_hat-t1*sample_sderror
  
  CI <- c(T_lo, T_up)
  
  return(CI)
}   


Basic <- function(bootstrap_estimates, bootstrap_sderrors, T_hat, sample_sderror, alpha) {
  B <- length(bootstrap_estimates)
  
  h1 <- ceiling(B * alpha/2)
  h2 <- floor(B * (1-alpha/2))
  
  bootstrap_estimates = sort(bootstrap_estimates)
  
  T_lo <- bootstrap_estimates[h1]
  T_up <- bootstrap_estimates[h2]
  
  CI <- c(T_lo, T_up)
  
  return(CI)
}

confidence_interval_names = c("BCa", "Bootstrap_t", "Basic")
intervals <- function(bootstrap_estimates, bootstrap_sderrors, T_hat, start_sample, sample_sderror, T_estimator, alpha){
  list(
    BCa(bootstrap_estimates, bootstrap_sderrors, T_hat, start_sample, sample_sderror, T_estimator, alpha),
    Bootstrap_t(bootstrap_estimates, bootstrap_sderrors, T_hat, sample_sderror, alpha),
    Basic(bootstrap_estimates, bootstrap_sderrors, T_hat, sample_sderror, alpha)
  )
}



