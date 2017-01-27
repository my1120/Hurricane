#' Pull the abbreviation of cities based on exposure criterion
#'
#' @inheritParams readStorm
#'
#' @export
pull_cities_name <- function(criterion = c()){
  path <- paste0("exposure/", criterion)
  x <- list.files(path)
  cities_name <- gsub(".rds", "", x = x)
  return(cities_name)
}
