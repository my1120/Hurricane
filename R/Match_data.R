#' Generate a case-crossover datasets.
#'
#' Generate a data frame for one city with storm days, lagged days, and matched
#' control days (non-storm days). The data frame will be directly used for
#' \code{crossbasis()} function in the \code{dlnm} package.
#'
#' @inheritParams readCity
#' @inheritParams readStorm
#'
#' @param control_ratio A integer vector giving the number of control days for
#' one storm day.
#' @param lags A integer vector giving number of lagged days for one storm day.
#' @param storm_id A character vector giving the storm ID of one specific storm
#' when you want to investigate the effect of this storm only.
#'
#' @return This function returns a dataframe with first \code{l} (l is equal to
#' the value of \code{lags} argument which is 14 by defauls) rows of NA, and
#' \code{n} stratums datasets. \code{n} is the number of storms if default
#' \code{storm_id} is used, otherwise there is only one stratum. Each stratum
#' has one row of storm day, l rows of lagged days, and 15 (by default) rows of
#' matched control days.
#'
#' @examples
#' \dontrun{
#' Match_data(root = "~/tmp/NMMAPS/",
#'               criterion = "rain75", city = "miam", storm_id = "Irene-1999")
#' }
#'
#'
#' @export
Match_data <- function(root = "~/tmp/NMMAPS/", criterion, city,
                          control_ratio = 10,
                          lags = 7, storm_id = NA){
  #print(city)
  ## generate the data
  df <- CityStorm(root, criterion, city)

  if(!is.na(storm_id)){
    ## exclude other storms
    df$hurr[df$storm_id != storm_id] <- 0
  }else{
    df <- df
  }

  ## exclude the 3 days within any other storm
  df$time <- 1:length(df$hurr)
  cand_control <- unique(c(which(df$hurr == 1) , which(df$hurr == 1) + 1,
                           which(df$hurr == 1) - 1))

  df$cand_control <- TRUE
  df$cand_control[cand_control] <- FALSE

  ## exclude the two weeks following 2001-9-11
  two_week_911 <- seq(as.Date("2001-09-11"),  as.Date("2001-09-11") + 14, by = 1)
  df$excu_911 <- ifelse(df$date %in% two_week_911, FALSE, TRUE)

  case_dates <- subset(df, hurr == 1) # hurr is exposure
  control_dates <- subset(df, hurr == 0)

  for(i in 1:nrow(case_dates)){
    ## choose lags of storm days (lag0)
    lag_dates <- case_dates[i, ]$date + -2:lags
    lag_case <- subset(df, date %in% lag_dates)

    ## choose controls for storm days (lag0)
    control_range <- case_dates[i, ]$doy + -3:3
    control_subset <- subset(control_dates,
                             control_dates$year != case_dates[i, ]$year &
                             doy %in% control_range &
                             cand_control & excu_911)

    ## use a repeat loop to sample controls in order to exclude case day in
    ## the lagged period of any control.
    ## but it's possible to have a storm day in the lagged period of the other
    ## storm day (I don't think it matters).
    repeat(
      {
    controls <- dplyr::sample_n(control_subset, control_ratio)

    ## lagged controls
    la_con <- c(-2, -1, 1:lags)
    for(j in 1:length(la_con)){
      lag_control_dates <- controls$date + la_con[j]
      lag_control_each <- subset(df, date %in% lag_control_dates)

      if(j == 1){
        lag_control <- lag_control_each
      }else{
        lag_control <- rbind(lag_control, lag_control_each)
      }
    }

    if(all(lag_control$hurr == 0)){
      break
    }
      }
    )

    i_stratum <- rbind(lag_case, controls, lag_control)

    stratum <- paste("stratum", i, sep = ".")
    i_stratum$stratum <- stratum

    status <- c(rep("case", lags + 3), rep("control", control_ratio*(lags + 1 + 2)))
    # case: 1 storm day + 2 previous days + #lags day
    i_stratum$status <- status

    lag <- c(-2:lags, rep(0, control_ratio),
             rep(c(-2, -1, 1:lags), each = control_ratio))
    i_stratum$lag <- lag

    if(i == 1){
      new_df <- i_stratum
    }else{
      new_df <- rbind(new_df, i_stratum)
    }
  }

  ## rbind a "fake" data frame with lags = 14 rows and same number of columns
  ## as "new_df"
  #coln <- ncol(new_df)
  #be_data <- as.data.frame(matrix(0, nrow = lags, ncol = coln))
  #colnames(be_data) <- colnames(new_df)
  #be_data$date <- seq(as.Date("2008/1/1"), by = "day", length.out = lags)

  #df_to_mod <- rbind(be_data, new_df)

  return(new_df)
}

