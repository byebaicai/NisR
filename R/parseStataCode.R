#' Parsing Stata Code into R
#'
#' This function loads the NIS Stata Loading code into a format the R can read
#' @param statafile string, path to stata loading file
#' @export
parseStataCode <- function(statafile){
  options(stringsAsFactors=FALSE)
  statacode <- readLines(statafile)
  indStart <- min(grep('infix', statacode))
  indEnd <- min(grep('using', statacode))-1
  codelines <- statacode[indStart:indEnd]
  codelines[1] <- gsub('infix','     ', codelines[1])
  codelines = stringr::str_trim(codelines)
  types <- str_extract(codelines, '^\\w+')
  translate <- c('int'='integer', 'byte'='integer', 'str'='string', 'double'='double', 'long'='double')
  types2 <- translate[types]
  varnames <- str_extract(codelines, '[A-Z]\\w+')
  startcols <- as.integer(str_extract(codelines,' \\d+'))
  maxcols = unlist(str_extract_all(codelines,'\\d+'))
  startcols = c(startcols, as.integer(maxcols[length(maxcols)]))
  startcols[length(startcols)] =  startcols[length(startcols)] + 1
  widths = diff(startcols)
  return(data.frame(varnames = varnames, widths=widths, classes=types2))
}
