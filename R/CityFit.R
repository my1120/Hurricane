#' Fit distributed lag model for one city.
#'
#' This function uses dataframe created by \code{CrossoverData} and fits a
#' distributed lag model for this city.
#'
#' @inheritParams readCity
#' @inheritParams readStorm
#' @inheritParams CrossoverData
#' @inheritParams dlnm::crossbasis
#'
#' @param cause A character string giving the death outcome used in the model.
#'
#' @return Prints the abbreviation of city's name and returns a list of
#' \code{glm()} model object and predicted results by \code{crosspred()}
#' function from \code{dlnm} package.
#'
#' @examples
#' \dontrun{
#' fit_detroit <- CityFit(root = "~/tmp/NMMAPS/", criterion = "rain50",
#'                        city = "det", cause = "accident",
#'                        arglag = list(fun = "integer"))
#'
#' lag_knots <- logknots(14, 4)
#' fit_detroit <- CityFit(root = "~/tmp/NMMAPS/", criterion = "rain50",
#'                        city = "det", cause = "accident",
#'                        arglag = list(fun = "ns", knots = lag_knots))
#' }
#'
#' @export
CityFit <- function(root = "~/tmp/NMMAPS/", criterion, city, cause = "all",
                    control_ratio = 15, lags = 14,
                    storm_id = NA, arglag){

  df <- CrossoverData(root, criterion, city,
                      control_ratio, lags, storm_id)

  cb <- dlnm::crossbasis(df$hurr, lag = c(-2, lags),
                         argvar = list(fun = "lin"),
                         arglag = arglag)

  # if this city has only one storm, "stratum" will not be included in the model.
  if(nrow(subset(df, hurr == 1)) == 1){
    city_fit <- glm(df[, cause] ~ cb + splines::ns(year, 2),
                    family = quasipoisson(link = log),
                    data = df,
                    control = glm.control(epsilon = 10E-8, maxit = 5000))
  }else{
    city_fit <- glm(df[, cause] ~ cb + splines::ns(year, 2) + stratum,
                    family = quasipoisson(link = log),
                    data = df,
                    control = glm.control(epsilon = 10E-8, maxit = 5000))
  }

  city_pred <- dlnm::crosspred(basis = cb, model = city_fit, at = 1)

  fit_city <- list(city_fit = city_fit, city_pred = city_pred)
  return(fit_city)
}
