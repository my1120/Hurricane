#' Pull city-level characteristics
#'
#'
#' @param city_list A vector of abbreviation of cities' names.
#'
#'
#' @return This function returns a dataframe giving city-level variables for all the cities in the \code{city_list}.
#'
#'
#' @export
pull_cities_var <- function(city_list){
  char.city <- readRDS("data/county.rds")

  spe_city_var <- subset(char.city, city %in% city_list)
  spe_city_var <- spe_city_var[, c("city", "citynameU", "pop")] %>%
                    mutate(pop = as.numeric(pop)) %>%
                    group_by(city) %>%
                    summarise(pop = sum(pop))

  return(spe_city_var)
}
