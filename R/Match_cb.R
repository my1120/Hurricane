#' Select matched observations from original $crossbasis$ matrix.
#'
#'
#' @inheritParams readCity
#' @inheritParams readStorm
#' @inheritParams CityStorm
#' @inheritParams Match_data
#' @inheritParams dlnm::crossbasis
#'
#' @return This function returns $crossbasis$ object.
#'
#' @importFrom dplyr %>%
#'
#' @export
Match_cb <- function(root = "~/tmp/NMMAPS/", criterion, city,
                       control_ratio = 10, lags = 7,
                       arglag = list(fun = "integer")){

  # crossbasis with whole datase
  orig_data <- CityStorm(root, criterion, city)
  orig_cb <- crossbasis(orig_data$hurr, lag = c(-2, lags),
                        argvar = list(fun = "lin"),
                        arglag = arglag)

  obs_n <- nrow(orig_data)
  orig_cb_matr <- as.data.frame(subset(orig_cb, nrow = obs_n))
  orig_cb_matr$date <- orig_data$date

  # matched dataset
  matched_date <- Match_data(root, criterion, city,
                                control_ratio, lags) %>%
                   select(date)


  matched_cb_matrix <- orig_cb_matr %>%
    right_join(matched_date, by = "date") %>%
    select(-date) %>%
    as.matrix()

  # add attributes to matched_cb
  matched_dim <- dim(matched_cb_matrix)
  attr <- attributes(orig_cb)
  attr$dim <- matched_dim

  matched_cb <- matched_cb_matrix
  attributes(matched_cb) <- attr

  return(list("cb" = matched_cb, "matrix" = matched_cb_matrix,
              "attributes" = attr))
}
