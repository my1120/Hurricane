MetaReg <- function(city_list = c(), criterion = c(), cause = "all",
                       lags = 14, arglag, storm_id = NA,
                       method = "reml"){

  city_fit <-  purrr::map(city_list, function(x) CityFit(criterion = criterion,
                                                         city = x,
                                                         cause = cause,
                                                         lags = lags,
                                                         arglag = arglag,
                                                         storm_id = storm_id))
  city_coefs <- purrr::map(city_fit, pull_city_coef)
  city_coefs <- do.call("rbind", city_coefs)

  city_vcovs <- purrr::map(city_fit, pull_city_vcov)

  meta_fit <- mvmeta::mvmeta(city_coefs, S = city_vcovs, method = method)

  return(meta_fit)
}
