#' This function fits a mixed model with an random intercept for $community$
#' (i.e. $city$). Other covariates are indicator
#' variable for $year$ and $dow$. The default optimizer is used in $glmer$.
#'
#'
#' @inheritParams lme4::glmer
#' @inheritParams dlnm::crosspred
#'
#' @return Thins function returns a mixed model and the $crosspred$ object.
#'
#' @export
Mixed_bobyqa <- function(cb_to_mod, data_to_mod, cause){
  fit <- lme4::glmer(data_to_mod[, cause] ~ cb_to_mod + factor(year) +
                 dow + (1|city),
               data = data_to_mod,
               offset = log(pop),
               family = poisson(link = "log"),
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5)))

  pred <- dlnm::crosspred(cb_to_mod, fit, at = 1)

  return(list("fit" = fit, "pred" = pred))
}

#' This function fits a mixed model with an random intercept for $community$
#' (i.e. $city$). Other covariates are indicator
#' variable for $year$ and $dow$. The $Nelder_Mead$ optimizer is used in $glmer$.
#'
#'
#' @inheritParams lme4::glmer
#' @inheritParams dlnm::crosspred
#'
#' @return Thins function returns a mixed model and the $crosspred$ object.
#'
#' @export
Mixed_nm <- function(cb_to_mod, data_to_mod, cause){
  fit <- lme4::glmer(data_to_mod[, cause] ~ cb_to_mod + factor(year) +
                       dow + (1|city),
                     data = data_to_mod,
                     offset = log(pop),
                     family = poisson(link = "log"),
                     control = glmerControl(optimizer = "Nelder_Mead",
                                            optCtrl = list(maxfun = 2e5)))

  pred <- dlnm::crosspred(cb_to_mod, fit, at = 1)

  return(list("fit" = fit, "pred" = pred))
}


#' This function fits a mixed model with an random intercept for $community$
#' (i.e. $city$). Other covariates are: a linear function for $year$, and an
#' indicator variable for $dow$.
#'
#'
#' @inheritParams lme4::glmer
#' @inheritParams dlnm::crosspred
#'
#' @return Thins function returns a mixed model and the $crosspred$ object.
#'
#' @export
Mixed_2 <- function(cb_to_mod, data_to_mod, cause){

  fit <- lme4::glmer(data_to_mod[, cause] ~ cb_to_mod + year_s +
                 dow + (1|city),
               data = data_to_mod,
               offset = log(pop),
               family = poisson(link = "log"),
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5)))

  pred <- dlnm::crosspred(cb_to_mod, fit, at = 1)

  return(list("fit" = fit, "pred" = pred))
}
