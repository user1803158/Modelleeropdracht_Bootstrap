library("plyr")
source("confidence_intervals.r")


param_bootstrap_data <- function(distr, n, N, estimator){
  replicate(N, estimator(distr(n)))
}


analyse_bootstrap_data <- function(true_theta, alphas, true_variance, M, estimator, bootstrap_data_generator){
  # apply the bootstrap algorithm M times and put some results in the 'bootstrap_results' dataframe
  bootstrap_results = rdply(M, .id=NULL, .expr={
    bootstrap_data = bootstrap_data_generator()
    interval_pairs = unlist(lapply(alphas, function(alpha){
      intervals(bootstrap_data$estimates, bootstrap_data$stderrors, bootstrap_data$theta_hat, bootstrap_data$start_sample, bootstrap_data$sample_stderror, estimator, alpha)
    }), recursive=F)
    truth_values = vapply(interval_pairs, function(x) (x[1] <= true_theta) & (true_theta <= x[2]), numeric(1))
    c(var(bootstrap_data$estimates), truth_values)
  })
  
  # turn bootstrap_results into a nice results list
  results = list(mean((bootstrap_results[,1]-true_variance)^2))
  for (i in seq_along(alphas)){
    for (j in seq_along(confidence_interval_names)){
      collumn_index = 2+(i-1)*length(confidence_interval_names)+(j-1)
      results = c(results, 1-mean(bootstrap_results[, collumn_index]))
    }
  }
  result_names = c("mean square error of variance")
  for (alpha in alphas){
    for (ci_name in confidence_interval_names){
      name = paste(ci_name, ";alpha=", format(round(alpha, digits=3), nsmall=3), sep="")
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
  true_distr = true_distr_gen(true_params)
  true_variance = var(param_bootstrap_data(true_distr, n, true_variance_N, estimator))
  analyse_bootstrap_data(true_theta, alphas, true_variance, M, estimator, function(){
    start_sample = true_distr(n)
    distr = bootstrap_distr_gen(params_estimator(start_sample))
    bootstrap_results = raply(N, {
      bootstrap_sample = distr(n)
      c(estimator(bootstrap_sample), stderror(bootstrap_sample))
    })
    list(
      theta_hat=estimator(start_sample),
      sample_stderror=stderror(start_sample),
      start_sample=start_sample,
      estimates=bootstrap_results[,1], 
      stderrors=bootstrap_results[,2]
    )
  })
}

nonparam_monster <- function(true_distr, estimator, true_theta, n, M, N, true_variance_N, alphas=c()){
  true_variance = var(param_bootstrap_data(true_distr, n, true_variance_N , estimator))
  analyse_bootstrap_data(true_theta, alphas, true_variance, M, estimator, function(){
    start_sample = true_distr(n)
    bootstrap_results = raply(N, {
      bootstrap_sample = sample(start_sample, n, replace=T)
      c(estimator(bootstrap_sample), stderror(bootstrap_sample))
    })
    list(
      theta_hat=estimator(start_sample),
      sample_stderror=stderror(start_sample),
      start_sample=start_sample,
      estimates=bootstrap_results[,1], 
      stderrors=bootstrap_results[,2]
    )
  })
}
