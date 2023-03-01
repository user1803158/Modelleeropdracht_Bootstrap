library("plyr")
source("confidence_intervals.r")


param_bootstrap_data <- function(distr, n, N, estimator){
  replicate(N, estimator(distr(n)))
}

nonparam_bootstrap_data <- function(data, N, estimator){
  replicate(N, estimator(sample(data, length(data), replace=T)))
}


param_monster <- function (true_distr_gen, true_params, true_theta, params_estimator, n, M, N, true_variance_N, alphas=c(), estimator=NULL, bootstrap_distr_gen=NULL){
  if (is.null(estimator)){
    estimator = params_estimator
  }
  if (is.null(bootstrap_distr_gen)){
    bootstrap_distr_gen = true_distr_gen
  }
  
  # calculate the true varaince
  true_variance = var(param_bootstrap_data(true_distr_gen(true_params), n, true_variance_N, estimator))
  
  # apply the bootstrap algorithm M times and put some results in the 'bootstrap_results' dataframe  
  true_distr = true_distr_gen(true_params)
  bootstrap_results = rdply(M, {
    params_hat = params_estimator(true_distr(n))
    data = param_bootstrap_data(bootstrap_distr_gen(params_hat), n, N, estimator)
    interval_pairs = unlist(lapply(alphas, function(x) intervals(data, x)), recursive=F)
    truth_values = vapply(interval_pairs, function(x) (x[[1]] <= true_theta) & (true_theta <= x[[2]]), numeric(1))
    c(var(data), truth_values)
  }, .id=NULL)
  
  # turn bootstrap_results into a nice results list
  results = list(mean((bootstrap_results[,1]-true_variance)^2))
  for (i in 0:(length(alphas)-1)){
    for (j in 0:(length(confidence_interval_names)-1)){
      collumn_index = 1+i*length(alphas)+j
      results = c(results, mean(bootstrap_results[, collumn_index]))
    }
  }
  result_names = c("mean_square_error_of_variance")
  # TODO suff here
  names(results) = result_names
  results
}

nonparam_monster <- function(true_distr, estimator, n, M, N){
  replicate(M, var(nonparam_bootstrap_data(true_distr(n), N, estimator)))
}
