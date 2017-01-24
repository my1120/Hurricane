#' Read NMMAPS data for each city
#'
#' @param city A character string giving abbreviation of city name.
#' @param collapseAge Logic value specifying whether to aggregate records based
#'  on age group.
#'
#' @return df This function returns a dataframe containing date, death counts,
#' etc.
#'
#' @export
readCity <- function(city = c(), collapseAge = TRUE){
  root <- "~/tmp/NMMAPS/" # local path where the NMMAPS data is stored
  file <- paste(root, city, ".rds", sep = "")
  df <- readRDS(file)

  if(collapseAge == TRUE){
    df <- rowsum(df[,c("alldeath", "death", "accident", "cvd", "resp",
                       "copd", "suicide", "tmpd", "dptp")], df$date,
                 reorder = FALSE)
    df$date <- as.Date(rownames(df))
    df[ , c("tmpd", "dptp")] <- df[ , c("tmpd", "dptp")]/3
    df$dow <- as.factor(as.POSIXlt(df$date)$wday)
  }

  return(df)
}
#' @examples
#  miam <- readCity(city = "miam", collapseAge = TRUE)
