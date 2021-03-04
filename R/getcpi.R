
#' Scrapes Consumer Price Index from Bureau of Labor Statistics
#'
#' This function allows you to pull CPIs between the specified years from the BLS website and put it in a database for use.
#' @param startyear integer, start year for pulling CPIs
#' @param endyear integer, end year for pulling CPIs
#' @export
#' @examples
#' cpi <- getcpi(2008, 2014)
#' cpi

getcpi <- function(startyear, endyear, ...){
  return(blscrapeR::bls_api("CUSR0000SA0", startyear= startyear, endyear = endyear))
}
