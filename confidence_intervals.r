


intervals <- function(bootstrap_data, alpha){
  list(
    quantile_based=C(quantile(bootstrap_data, alpha/2), quantile(bootstrap_data, 1-alpha/2)),
  )
}



