

confidence_interval_names = c("quantile_based")

intervals <- function(bootstrap_data, alpha){
  list(
    c(quantile(bootstrap_data, alpha/2), quantile(bootstrap_data, 1-alpha/2))
  )
}



