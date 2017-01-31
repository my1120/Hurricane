#' Pull Coefficients matrix.
#'
#' This function pulls coefficients matrix from \code{crosspred()}
#' object.
#'
#' @param CityFit A fitted model object.
#'
#' @return A matrix of coefficients between the parameters estimates in the
#' model.
#'
#' @export
pull_city_coef <- function(CityFit){
  city_coef <- coef(CityFit[["city_pred"]])
  return(city_coef)
}


#' Pull Variance-Covariance matrix.
#'
#' This function pulls variance-covariance matrix from \code{crosspred()}
#' object.
#'
#' @param CityFit A fitted model object.
#'
#' @return A matrix of variance-covariance between the parameters estimates in
#' the model.
#'
#' @export
pull_city_vcov <- function(CityFit){
  city_vcov <- vcov(CityFit[["city_pred"]])
  return(city_vcov)
}


#' Univariate Meta-regression.
#'
#' This function performs a univariate meta-regression using \code{mvmeta()}
#' function in the \code{mvmeta} package.
#'
#' @inheritParams readStorm
#' @inheritParams CityFit
#' @inheritParams mvmeta::mvmeta
#'
#' @param city_list A list of abbreviation of cities' names
#'
#' @return Returns a list of class \code{'mvmeta'}.
#'
#' @references
#'
#' Gasparrini A., Armstrong, B., Kenward M. G. (2012). Multivariate
#' meta-analysis for non-linear and other multi-parameter associations.
#' Statistics in Medicine. 31(29):3821-3839.
#'
#' @examples
#' \dontrun{
#' city_list <- c("no", "lkch", "miam", "jckv", "mobi")
#' meta <- UniMetaReg(city_list = city_list, criterion = "rain75",
#'                    cause = "accident",
#'                    arglag = list(fun = "integer"))
#' }
#'
#' @export
UniMetaReg <- function(city_list = c(), criterion = c(), cause = "all",
                       arglag, method = "reml"){

  city_fit <-  purrr::map(city_list, function(x) CityFit(criterion = criterion,
                                                         city = x,
                                                         cause = cause,
                                                         arglag = arglag))
  city_coefs <- purrr::map(city_fit, pull_city_coef)
  city_coefs <- do.call("rbind", city_coefs)

  city_vcovs <- purrr::map(city_fit, pull_city_vcov)

  meta_fit <- mvmeta::mvmeta(city_coefs ~ 1, S = city_vcovs, method = method)

  return(meta_fit)
}


#' Prediction for a meta-regression
#'
#' This function uses \code{crosspred()} function in the \code{dlnm} package to
#' predict the
#'
#' @param meta_model A model object of class \code{'mvmeta'}.
#' @param exposure A numeric vector giving the exposure value for prediction.
#'
#' @return Returns a list of class \code{'crosspred'}.
#'
#' @examples
#' \dontrun{
#' exposure <- c(rep(0, 20), 1, rep(0, 20))
#' pred <- pred_meta(meta_model = meta, exposure = exposure)
#' }
#'
#' @export
pred_meta <- function(meta_model, exposure = c()){
  hurr_basis <- dlnm::crossbasis(x = exposure,
                                 lag = c(0, 14),
                                 argvar = list(fun = "lin"),
                                 arglag = list(fun = "ns",
                                               knots = dlnm::logknots(14, 4)))

  meta_pred <- crosspred(hurr_basis, coef = coef(meta_model),
                         vcov = vcov(meta_model), model.link = "log",
                         cen = 0, at = 1)
  return(meta_pred)
}


#' Plot the prediction of a meta-regression.
#'
#' @param meta_pred A \code{'crosspred'} class object.
#' @param title A character string giving the title of the plot.
#'
#' @export
plot_meta <- function(meta_pred, title = c()){
  plot(meta_pred, xlab = "Lag",
       ylab = "RR compared to non-storm day",
       exp = TRUE, ptype = "slices", var = 1, main = title)
}
