
#' Adjusting NIS database for inflation
#'
#' This function adjusts NIS database extracted using a loadnis function for inflation. Must have extracted the 'TOTCHG' and 'AMONTH' variables from the Core file
#' @param nt data frame, NIS data frame extracted using a loadnis function. Must have extracted the 'TOTCHG' and 'AMONTH' variables from the Core file
#' @param cpi data frame, table of CPI values scraped from BLS using getcpi function
#' @param new_cpi numeric, single CPI from date you are adjusting your NIS charge values to (i.e. today's CPI to get today's dollar value)
#' @export
#' @examples
#' nt<- loadnis_12_14(wt, d, ho, h, sv, s, corevar, filtervar, codes, sevvar, hospvar)
#' cpi<- getcpi(2012, 2014)
#' new_cpi<- 262.0
#' nadj<- nis_adjchg(nt, cpi, new_cpi)
nis_adjchg<- function(nt, cpi, new_cpi, ...){
  nt$NMONTH<- factor(nt$AMONTH, levels = c(-9,1,2,3,4,5,6,7,8,9,10,11,12), labels = c(NA,'January',"February",'March','April','May','June','July', 'August','September','October','November','December'))
  ncpi<- nt%>%
    dplyr::left_join(cpi, by = c('YEAR' = 'year', 'NMONTH' = 'periodName'))
  return(ncpi %>%
           mutate(ADJCHG = (TOTCHG /value)*as.numeric(new_cpi))
  )
}
