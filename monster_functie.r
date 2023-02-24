

param.bootstrap <- function(distr, theta_hat, n, N, estimator){
  f = distr(theta_hat)
  theta_hat_hats = replicate(N, estimator(f(n)))
  var(theta_hat_hats)
}

nonparam.bootstrap <- function(data, n, N, estimator){
  
}



param.monster <- function (true_distr, true_theta, params_estimator, n, M, N, estimator=NULL, bootstrap_distr=NULL){
  if (is.null(estimator)){
    estimator = params_estimator
  }
  if (is.null(bootstrap_distr)){
    bootstrap_distr = true_distr
  }
  f = true_distr(true_theta)
  replicate(M, param.bootstrap(bootstrap_distr, params_estimator(f(n)), n, N, estimator))
}

