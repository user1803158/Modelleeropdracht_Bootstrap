
source("confidence_intervals.r")


param_bootstrap_data <- function(distr, n, N, estimator){
  replicate(N, estimator(distr(n)))
}

nonparam_bootstrap_data <- function(data, N, estimator){
  replicate(N, estimator(sample(data, length(data), replace=T)))
}


param_monster <- function (true_distr_gen, true_params, params_estimator, n, M, N, alphas=NULL, estimator=NULL, bootstrap_distr_gen=NULL){
  if (is.null(estimator)){
    estimator = params_estimator
  }
  if (is.null(bootstrap_distr_gen)){
    bootstrap_distr_gen = true_distr_gen
  }
  true_distr = true_distr_gen(true_params)
  replicate(M, {
    params_hat = params_estimator(true_distr(n))
    data = param_bootstrap_data(bootstrap_distr_gen(params_hat), n, N, estimator)
    var(data)
  })
}

nonparam_monster <- function(true_distr, estimator, n, M, N){
  replicate(M, var(nonparam_bootstrap_data(true_distr(n), N, estimator)))
}
