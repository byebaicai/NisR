
#' Load NIS files from 2008 to 2011
#'
#' This function loads NIS databases from the years 2008 to 2011 using provided diagnosis or procedure codes.
#' Requires .ASC files for the NIS, including: Core File, Severity file, Hospital File, Hospital Weights file and corresponding Stata Load files for all of the above
#' @param wt string, Path to Core Stata Load file
#' @param d string, Path to Core .ASC file
#' @param ld string, Path to Hospital Trend Weight Stata Load file
#' @param t string, Path to Hospital Trend Weight .ASC file
#' @param ho string, Path to Hospital Stata Load file
#' @param h string, Path to Hospital .ASC file
#' @param sv string, Path to Severity Stata Load file
#' @param s string, Path to Severity .ASC file
#' @param corevar list of strings, list of variable names you want to pull from the NIS Core file, MUST INCLUDE the variables 'HOSPID' and 'KEY'
#' @param filtervar list of strings, list of variables you want to filter on, diagnosis or procedure, MUST START WITH EITHER DX OR PR
#' @param codes list of strings or integers, list of diagnosis or procedure ICD 9 codes to filter from
#' @param sevvar list of strings, list of variable names you want to pull from the NIS Severity file, MUST INCLUDE the variable 'KEY'
#' @param hospvar list of strings, list of variable names you want to pull from the NIS Hospital file, MUST INCLUDE the variable 'HOSPID'
#' @export
#' @examples
#' wt <- '../StataLoad_NIS_2009_Core.Do'
#' d <- '../NIS_2009_Core.ASC'
#' ld <- '../StataLoad_NIS_2009_HOSPITAL_TrendWt.Do'
#' t <- '../NIS_2009_HOSPITAL_TrendWt.ASC'
#' ho <- '../StataLoad_NIS_2009_Hospital.Do'
#' h <- '../NIS_2009_Hospital.ASC'
#' sv <- '../StataLoad_NIS_2009_Severity.Do'
#' s <- '../NIS_2009_Severity.ASC'
#' corevar <- c('KEY','HOSPID','DX1','DX2', 'PR1','PR2','YEAR','AMONTH','AGE','LOS', 'DIED','FEMALE','TOTCHG','RACE','PAY1','ZIPINC_QRTL')
#' filtervar <- c('PR1', 'PR2')
#' codes <- c(8555, 8687, '8690')
#' sevvar <- c('KEY', 'APRDRG', 'APRDRG_Risk_Mortality', 'APRDRG_Severity', 'CM_AIDS')
#' hospvar <- c('HOSPID','HOSP_BEDSIZE','H_CONTRL','HOSP_LOCTEACH','HOSP_REGION')
#' n2009<- loadnis_08_11(wt, d, ld, t, ho, h, sv, s, corevar, filtervar, codes, sevvar, hospvar)
#' head(n2009)

loadnis_08_11 <- function(wt, d, ld, t, ho, h, sv, s, corevar, filtervar, codes, sevvar, hospvar, ...){
  wt <- parseStataCode(wt)
  d <- LaF::laf_open_fwf(d, column_widths=wt$widths, column_types=wt$classes, column_names = wt$varnames)

  ld <- parseStataCode(ld)
  t <- laf_open_fwf(t, column_widths=ld$widths, column_types=ld$classes, column_names = ld$varnames)

  ho <- parseStataCode(ho)
  h <- laf_open_fwf(h, column_widths=ho$widths, column_types=ho$classes, column_names = ho$varnames)

  sv <- parseStataCode(sv)
  s <- laf_open_fwf(s, column_widths=sv$widths, column_types=sv$classes, column_names = sv$varnames)

  return(d[,corevar] %>%
           dplyr::filter_at(vars(all_of(filtervar)), any_vars(. %in% codes))%>%
           left_join(t[,c('HOSPID', 'TRENDWT')], by ='HOSPID') %>%
           left_join(s[,sevvar], by = 'KEY') %>%
           left_join(h[,hospvar], by = 'HOSPID')
  )
}
