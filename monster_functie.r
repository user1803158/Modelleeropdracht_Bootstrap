

param.bootstrap <- function(distr_gen, theta_hat, n, N, estimator){
  distr = distr_gen(theta_hat)
  theta_hat_hats = replicate(N, estimator(distr(n)))
  var(theta_hat_hats)
}

nonparam.bootstrap <- function(data, n, N, estimator){
  theta_hat_hats = replicate(N, estimator(sample(data, n, replace=T)))
  var(theta_hat_hats)
}



param.monster <- function (true_distr_gen, true_theta, params_estimator, n, M, N, estimator=NULL, bootstrap_distr_gen=NULL){
  if (is.null(estimator)){
    estimator = params_estimator
  }
  if (is.null(bootstrap_distr_gen)){
    bootstrap_distr_gen = true_distr_gen
  }
  true_distr = true_distr_gen(true_theta)
  replicate(M, param.bootstrap(bootstrap_distr_gen, params_estimator(true_distr(n)), n, N, estimator))
}

nonparam.monster <- function(true_distr, estimator, n, M, N){
  replicate(M, nonparam.bootstrap(true_distr(n), n, N, estimator))
}
