library("plyr")
source("confidence_intervals.r")


param_bootstrap_data <- function(distr, n, N, estimator){
  replicate(N, estimator(distr(n)))
}


analyse_bootstrap_data <- function(true_T, alphas, true_variance, M, T_estimator, bootstrap_data_generator){
  # apply the bootstrap algorithm M times and put some results in the 'bootstrap_results' dataframe
  bootstrap_results = rdply(M, .id=NULL, .expr={
    bootstrap_data = bootstrap_data_generator()
    interval_pairs = unlist(lapply(alphas, function(alpha){
      intervals(bootstrap_data$T_estimates, bootstrap_data$stderrors, bootstrap_data$T_hat, bootstrap_data$start_sample, bootstrap_data$sample_stderror, T_estimator, alpha)
    }), recursive=F)
    truth_values = vapply(interval_pairs, function(x) (x[1] <= true_T) & (true_T <= x[2]), numeric(1))
    widths = vapply(interval_pairs, function(x) x[2]-x[1], numeric(1))
    c(var(bootstrap_data$T_estimates), rbind(truth_values, widths))
  })
  
  # turn bootstrap_results into a nice results list
  results = list(mean((log(bootstrap_results[,1])-log(true_variance))^2))
  for (i in seq_along(alphas)){
    for (j in seq_along(confidence_interval_names)){
      collumn_index = 2+2*((i-1)*length(confidence_interval_names)+(j-1))
      results = c(results, 1-mean(bootstrap_results[, collumn_index]), mean(bootstrap_results[, collumn_index+1]))
    }
  }
  result_names = c("MSE of log variance")
  for (alpha in alphas){
    for (ci_name in confidence_interval_names){
      name1 = paste(ci_name, "_", format(round(alpha, digits=3), nsmall=3), "_confidence", sep="")
      name2 = paste(ci_name, "_", format(round(alpha, digits=3), nsmall=3), "_avg_width", sep="")
      result_names = c(result_names, name1, name2)
    }
  }
  names(results) = result_names
  results
}


param_monster <- function (true_distr_gen, true_params, true_T, params_estimator, n, M, N, true_variance_N, alphas=c(), T_estimator, expected_distr_gen){
  # calculate the true variance of the estimator for T
  true_distr = true_distr_gen(true_params)
  true_variance = var(param_bootstrap_data(true_distr, n, true_variance_N, T_estimator))
  # apply the analyse_bootstrap_data function to a function that generates some
  # infomration about an application of the parametric bootstrap_algorithm
  analyse_bootstrap_data(true_T, alphas, true_variance, M, T_estimator, function(){
    # generate the initial sample
    start_sample = true_distr(n)
    # obtain the distribution with the estimated parameters
    distr = expected_distr_gen(params_estimator(start_sample))
    # apply the parametric bootstrap algorithm and store the estimation of T
    # for each bootstrap sample and the standard error of each bootstrap_sample
    bootstrap_results = raply(N, {
      bootstrap_sample = distr(n)
      c(T_estimator(bootstrap_sample), stderror(bootstrap_sample))
    })
    # return some information about this application of the bootstrap algorithm
    list(
      T_hat=T_estimator(start_sample),
      sample_stderror=stderror(start_sample),
      start_sample=start_sample,
      T_estimates=bootstrap_results[,1], 
      stderrors=bootstrap_results[,2]
    )
  })
}

nonparam_monster <- function(true_distr, T_estimator, true_T, n, M, N, true_variance_N, alphas=c()){
  # calculate the true variance of the estimator for T
  true_variance = var(param_bootstrap_data(true_distr, n, true_variance_N, T_estimator))
  # apply the analyse_bootstrap_data function to a function that generates some
  # information about an application of the bootstrap algorithm
  analyse_bootstrap_data(true_T, alphas, true_variance, M, T_estimator, function(){
    # generate the initial sample
    start_sample = true_distr(n)
    # apply the non-parametric bootstrap algorithm and store the estimation of
    # T for each bootstrap sample and the standard error of each 
    # bootstrap_sample
    bootstrap_results = raply(N, {
      bootstrap_sample = sample(start_sample, n, replace=T)
      c(T_estimator(bootstrap_sample), stderror(bootstrap_sample))
    })
    # return some information about this application of the bootstrap algorithm
    list(
      T_hat=T_estimator(start_sample),
      sample_stderror=stderror(start_sample),
      start_sample=start_sample,
      T_estimates=bootstrap_results[,1], 
      stderrors=bootstrap_results[,2]
    )
  })
}
