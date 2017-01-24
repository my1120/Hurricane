#' Read NMMAPS data for each city.
#'
#' @param root The local path where the NMMAPS data is located.
#' @param city A character string giving the abbreviation of city's name.
#' @param collapseAge Logic value specifying whether to aggregate records based
#'  on age group.
#'
#' @return This function returns a time-series dataframe containing date, death counts,
#' etc.
#'
#' @examples
#' readCity(root = "~/tmp/NMMAPS/", city = "miam")
#'
#' @export
readCity <- function(root = c(), city = c(), collapseAge = TRUE){
  root <- root
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
