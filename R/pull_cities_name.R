#' Pull the abbreviation of cities based on exposure criterion
#'
#' @inheritParams readStorm
#'
#' @return This function returns a character vector with abbreviation of
#' cities' name.
#'
#' @examples
#' \dontrun{
#' pull_cities_name("rain50")
#' }
#'
#' @export
pull_cities_name <- function(criterion = c()){
  path <- paste0("exposure/", criterion)
  x <- list.files(path)
  cities_name <- gsub(".rds", "", x = x)
  return(cities_name)
}
