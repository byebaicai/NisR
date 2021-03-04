
#' Load NIS files from 2012 to 2014
#'
#' This function loads NIS databases from the years 2012 to 2014 using provided diagnosis or procedure codes.
#' Requires .ASC files for the NIS, including: Core File, Severity file, Hospital File, Hospital Weights file and corresponding Stata Load files for all of the above
#' @param wt string, Path to Core Stata Load file
#' @param d string, Path to Core .ASC file
#' @param ho string, Path to Hospital Stata Load file
#' @param h string, Path to Hospital .ASC file
#' @param sv string, Path to Severity Stata Load file
#' @param s string, Path to Severity .ASC file
#' @param corevar list of strings, list of variable names you want to pull from the NIS Core file, MUST INCLUDE the variables 'HOSP_NIS' and 'KEY_NIS'
#' @param filtervar list of strings, list of variables you want to filter on, diagnosis or procedure, MUST START WITH EITHER DX OR PR
#' @param codes list of strings or integers, list of diagnosis or procedure ICD 9 codes to filter from
#' @param sevvar list of strings, list of variable names you want to pull from the NIS Severity file, MUST INCLUDE the variable 'KEY_NIS'
#' @param hospvar list of strings, list of variable names you want to pull from the NIS Hospital file, MUST INCLUDE the variable 'HOSP_NIS'
#' @export
#' @examples
#' wt <- '../StataLoad_NIS_2012_Core.Do'
#' d <- '../NIS_2012_Core.ASC'
#' ho <- '../StataLoad_NIS_2012_Hospital.Do'
#' h <- '../NIS_2012_Hospital.ASC'
#' sv <- '../StataLoad_NIS_2012_Severity.Do'
#' s <- '../NIS_2012_Severity.ASC'
#' corevar <- c('KEY_NIS','HOSP_NIS','DISCWT','DX1','DX2', 'PR1','PR2','YEAR','AMONTH','AGE','LOS', 'DIED','FEMALE','TOTCHG','RACE','PAY1','ZIPINC_QRTL')
#' filtervar <- c('PR1','PR2')
#' codes <- c(8555, 8687,'8690')
#' sevvar <- c('KEY_NIS','APRDRG', 'APRDRG_Risk_Mortality', 'APRDRG_Severity', 'CM_AIDS')
#' hospvar <- c('HOSP_NIS','HOSP_BEDSIZE','H_CONTRL','HOSP_LOCTEACH','HOSP_REGION')
#' n2012 <- loadnis_12_14(wt, d, ho, h, sv, s, corevar, filtervar, codes, sevvar, hospvar)
#' head(n2012)

loadnis_12_14 <- function(wt, d, ho, h, sv, s, corevar, filtervar, codes, sevvar, hospvar, ...){
  wt <- LaF::parseStataCode(wt)
  d <- laf_open_fwf(d, column_widths=wt$widths, column_types=wt$classes, column_names = wt$varnames)

  ho <- parseStataCode(ho)
  h <- laf_open_fwf(h, column_widths=ho$widths, column_types=ho$classes, column_names = ho$varnames)

  sv <- parseStataCode(sv)
  s <- laf_open_fwf(s, column_widths=sv$widths, column_types=sv$classes, column_names = sv$varnames)


  return(d[,corevar] %>%
           dplyr::filter_at(vars(all_of(filtervar)), any_vars(. %in% codes))%>%
           left_join(s[,sevvar], by = 'KEY_NIS') %>%
           left_join(h[,hospvar], by = 'HOSP_NIS')
  )
}
