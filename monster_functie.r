

# for now only parametric, outputs estimated variance of estimator at theta_hat
param.bootstrap <- function(distr, theta_hat, n, N, estimator){
  f = distr(theta_hat)
  theta_hat_hats = replicate(N, estimator(f(n)))
  var(theta_hat_hats)
}


monster <- function (true_distr, true_theta, bootstrap_distr, estimator, n, M, N){
  # todo
  
}




mean0norm <- function (sd){
  function(N){
    rnorm(N, mean=0, sd=sd)
  }
}

print(param.bootstrap(mean0norm, 1, 10, 1000, function(x) var(x)))
