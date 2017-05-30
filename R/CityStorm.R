#' Merge hurricane exposure to death outcomes.
#'
#' This function merges the two datasets created by readCity() function and
#' readStorm() function for one city by date.
#'
#' @inheritParams readCity
#' @inheritParams readStorm
#'
#' @return This function return a time-series dataframe containing daily death
#' counts and information about storms between 1988 to 2005 in this city.
#'
#' @importFrom dplyr %>%
#'
#' @examples
#' CityStorm(root = "~/tmp/NMMAPS/", criterion = "rain75", city = "miam")
#'
#' @export
CityStorm <- function(root = "~/tmp/NMMAPS/", criterion = c(), city = c()){
  # Get health data
  h.df <- readCity(root, city)

  # Get storm data
  s.df <- readStorm(criterion, city)

  storm_days <- s.df$date

  df <- dplyr::left_join(h.df, s.df, by = "date") %>%
         dplyr::mutate(hurr = ifelse(date %in% storm_days, 1, 0))

  df$hurr <- as.factor(df$hurr)

  # Add abbreviate and full cityname
  df$city <- city
  sub.county <- readRDS("data/sub.county.rds")

  cityname <- sub.county$citynameU[sub.county$city == city]
  df$cityname <- cityname

  #df$time <- scale(as.numeric(df$date), center = TRUE, scale = FALSE)
  #n.years <- length(unique(as.POSIXlt(df$date)$year))
  df$year <- lubridate::year(df$date)
  df$doy <- lubridate::yday(df$date)

  # Total death (all cause mortality including accident)
  df$all <- df$accident + df$death

  return(df)
}
