##' Reads MAVEN output as .csv or .xlsx file
##' 
##' @description Reads MAVEN output. File can be .xlsx or .csv, must have a column containing mz values
##' @param fn Filename (including path if necessary)
##' @param sheetIndex If Excel sheet, the index number of the worksheet
##' @return A data frame of whatever 
##' @export
##' 

read_mz_file <- function(fn="data/maven-output.csv", sheetIndex=1, col="medMz") {
  
  # Define functions to read files depending on the file extension
  xlsx <- function(fn, sheetIndex, col) {
    df <- read.xlsx(file=fn, sheetIndex=sheetIndex)
  }
  
  csv <- function(fn, colNum, ...) {
    df <- read.csv(file=fn)
  }
  
  # Determine whether file is .csv or .xlsx - lifted directly from ggsave
  save_fxn <- function(...) {
    pieces <- strsplit(fn, "\\.")[[1]]
    ext <- tolower(pieces[length(pieces)])
  }
  
  # Function to choose function to open files
  read_fun_picker <- function(fn) {
    pieces <- strsplit(fn, "\\.")[[1]]
    ext <- tolower(pieces[length(pieces)])
    match.fun(ext)
  }
  
  
  #browser()
  reader <- read_fun_picker(fn)
  df <- reader(fn, sheetIndex, colNum)
  
  df
  #df[ , col]
  
}