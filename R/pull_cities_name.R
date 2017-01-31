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
  px <- list.files(path)
  cities_name <- tools::file_path_sans_ext(px)
  return(cities_name)
}
