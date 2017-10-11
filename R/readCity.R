#' Read NMMAPS data for each city.
#'
#' @param root The local path where the NMMAPS data is located.
#' @param city A character string giving the abbreviation of city's name.
#' @param collapseAge Logic value specifying whether to aggregate records based
#'  on age group.
#' @param age_cat A character string giving the selected age categories.
#'
#' @return This function returns a time-series dataframe containing date, death counts,
#' etc.
#'
#' @examples
#' readCity(root = "~/Documents/NMMAPS/", city = "miam")
#'
#' @export
readCity <- function(root = "~/Documents/NMMAPS/", city = c(), collapseAge = TRUE,
                     age_cat = NULL){
  root <- root
  file <- paste(root, city, ".rds", sep = "")
  df <- readRDS(file)

  if(collapseAge == TRUE){
    df <- rowsum(df[,c("alldeath", "death", "accident", "cvd", "resp",
                       "copd", "suicide", "tmpd", "dptp")], df$date,
                 reorder = FALSE)
    df$date <- as.Date(rownames(df))
    df[ , c("tmpd", "dptp")] <- df[ , c("tmpd", "dptp")]/3
  }else{
    if(length(age_cat) == 1){
      df <- subset(df, agecat == age_cat)
      df <- rowsum(df[,c("alldeath", "death", "accident", "cvd", "resp",
                         "copd", "suicide", "tmpd", "dptp")], df$date,
                   reorder = FALSE)
      df$date <- as.Date(rownames(df))
    }else{
      df <- subset(df, agecat %in% age_cat)
      df <- rowsum(df[,c("alldeath", "death", "accident", "cvd", "resp",
                         "copd", "suicide", "tmpd", "dptp")], df$date,
                   reorder = FALSE)
      df$date <- as.Date(rownames(df))
      df[ , c("tmpd", "dptp")] <- df[ , c("tmpd", "dptp")]/2
    }
  }

  df$dow <- as.factor(as.POSIXlt(df$date)$wday)
  return(df)
}
