#' This function combine $matched crossbasis$.
#'
#'
#' @inheritParams Match_cb
#' @inheritParams readCity
#' @inheritParams readStorm
#' @inheritParams CityStorm
#' @inheritParams Match_data
#' @inheritParams dlnm::crossbasis
#'
#' @return This function returns matched crossbasis for a given citylist.
#'
#' @export
Join_matched_cb <- function(root = "~/tmp/NMMAPS/", citylist, criterion,
                            control_ratio = 10,
                            lag_1 = -2, lag_2 = 7,
                            arglag = list(fun = "integer")){

  cities_cb <- purrr::map(citylist, ~ Match_cb(root, criterion,
                                               city = .,
                                               control_ratio,
                                               lag_1, lag_2, arglag))

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
