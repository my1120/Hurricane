#' This function combine $matched crossbasis$.
#'
#'
#' @inheritParams Match_cb
#'
#' @export
Join_matched_cb <- function(citylist, criterion, control_ratio, lags,
                            arglag){

  cities_cb <- purrr::map(citylist, ~ Match_cb(criterion,
                                               city = .,
                                               control_ratio,
                                               lags, arglag))

  for(i in 1:length(citylist)){
    city_matrix <- cities_cb[[i]]$matrix

    if(i == 1){
      cities_matrix <- city_matrix
    }else{
      cities_matrix <- rbind(cities_matrix, city_matrix)
    }
  }

  cities_attr <- cities_cb[[1]]$attributes
  cities_attr$dim <- dim(cities_matrix)
  attributes(cities_matrix) <- cities_attr

  return(cities_matrix)
}
