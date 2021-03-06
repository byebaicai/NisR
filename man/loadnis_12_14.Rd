% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loadnis_12_14.R
\name{loadnis_12_14}
\alias{loadnis_12_14}
\title{Load NIS files from 2012 to 2014}
\usage{
loadnis_12_14(
  wt,
  d,
  ho,
  h,
  sv,
  s,
  corevar,
  filtervar,
  codes,
  sevvar,
  hospvar,
  ...
)
}
\arguments{
\item{wt}{string, Path to Core Stata Load file}

\item{d}{string, Path to Core .ASC file}

\item{ho}{string, Path to Hospital Stata Load file}

\item{h}{string, Path to Hospital .ASC file}

\item{sv}{string, Path to Severity Stata Load file}

\item{s}{string, Path to Severity .ASC file}

\item{corevar}{list of strings, list of variable names you want to pull from the NIS Core file, MUST INCLUDE the variables 'HOSP_NIS' and 'KEY_NIS'}

\item{filtervar}{list of strings, list of variables you want to filter on, diagnosis or procedure, MUST START WITH EITHER DX OR PR}

\item{codes}{list of strings or integers, list of diagnosis or procedure ICD 9 codes to filter from}

\item{sevvar}{list of strings, list of variable names you want to pull from the NIS Severity file, MUST INCLUDE the variable 'KEY_NIS'}

\item{hospvar}{list of strings, list of variable names you want to pull from the NIS Hospital file, MUST INCLUDE the variable 'HOSP_NIS'}
}
\description{
This function loads NIS databases from the years 2012 to 2014 using provided diagnosis or procedure codes.
Requires .ASC files for the NIS, including: Core File, Severity file, Hospital File, Hospital Weights file and corresponding Stata Load files for all of the above
}
\examples{
wt <- '../StataLoad_NIS_2012_Core.Do'
d <- '../NIS_2012_Core.ASC'
ho <- '../StataLoad_NIS_2012_Hospital.Do'
h <- '../NIS_2012_Hospital.ASC'
sv <- '../StataLoad_NIS_2012_Severity.Do'
s <- '../NIS_2012_Severity.ASC'
corevar <- c('KEY_NIS','HOSP_NIS','DISCWT','DX1','DX2', 'PR1','PR2','YEAR','AMONTH','AGE','LOS', 'DIED','FEMALE','TOTCHG','RACE','PAY1','ZIPINC_QRTL')
filtervar <- c('PR1','PR2')
codes <- c(8555, 8687,'8690')
sevvar <- c('KEY_NIS','APRDRG', 'APRDRG_Risk_Mortality', 'APRDRG_Severity', 'CM_AIDS')
hospvar <- c('HOSP_NIS','HOSP_BEDSIZE','H_CONTRL','HOSP_LOCTEACH','HOSP_REGION')
n2012 <- loadnis_12_14(wt, d, ho, h, sv, s, corevar, filtervar, codes, sevvar, hospvar)
head(n2012)
}
