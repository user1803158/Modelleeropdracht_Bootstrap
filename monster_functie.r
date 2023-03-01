library("plyr")
source("confidence_intervals.r")


param_bootstrap_data <- function(distr, n, N, estimator){
  replicate(N, estimator(distr(n)))
}

nonparam_bootstrap_data <- function(data, N, estimator){
  replicate(N, estimator(sample(data, length(data), replace=T)))
}


analyse_bootstrap_data <- function(true_theta, alphas, true_variance, M, bootstrap_data_generator){
  # apply the bootstrap algorithm M times and put some results in the 'bootstrap_results' dataframe
  bootstrap_results = rdply(M, .id=NULL, .expr={
    data = bootstrap_data_generator()
    interval_pairs = unlist(lapply(alphas, function(x) intervals(data, x)), recursive=F)
    truth_values = vapply(interval_pairs, function(x) (x[[1]] <= true_theta) & (true_theta <= x[[2]]), numeric(1))
    c(var(data), truth_values)
  })
  
  # turn bootstrap_results into a nice results list
  results = list(mean((bootstrap_results[,1]-true_variance)^2))
  for (i in 0:(length(alphas)-1)){
    for (j in 0:(length(confidence_interval_names)-1)){
      collumn_index = 2+i*length(confidence_interval_names)+j
      results = c(results, 1-mean(bootstrap_results[, collumn_index]))
    }
  }
  result_names = c("mean_square_error_of_variance")
  for (alpha in alphas){
    for (ci_name in confidence_interval_names){
      name = paste(ci_name, "alpha =", format(round(alpha, digits=3), nsmall=3))
      result_names = c(result_names, name)
    }
  }
  names(results) = result_names
  results
}


param_monster <- function (true_distr_gen, true_params, true_theta, params_estimator, n, M, N, true_variance_N, alphas=c(), estimator=NULL, bootstrap_distr_gen=NULL){
  if (is.null(estimator)){
    estimator = params_estimator
  }
  if (is.null(bootstrap_distr_gen)){
    bootstrap_distr_gen = true_distr_gen
  }
  # calculate the true variance
  true_variance = var(param_bootstrap_data(true_distr_gen(true_params), n, true_variance_N, estimator))  
  true_distr = true_distr_gen(true_params)
  analyse_bootstrap_data(true_theta, alphas, true_variance, M, function(){
    param_bootstrap_data(bootstrap_distr_gen(params_estimator(true_distr(n))), n, N, estimator)
  })
}

nonparam_monster <- function(true_distr, estimator, true_theta, n, M, N, true_variance_N, alphas=c()){
  true_variance = var(param_bootstrap_data(true_distr, n, true_variance_N ,estimator))
  analyse_bootstrap_data(true_theta, alphas, true_variance, M, function(){
    nonparam_bootstrap_data(true_distr(n), N, estimator)
  })
}
