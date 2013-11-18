##' Reads MAVEN output as .csv or .xlsx file
##' 
##' @description Reads MAVEN output. File can be .xlsx or .csv, must have a column containing mz values
##' @param fn Filename (including path if necessary)
##' @param sheetIndex If Excel sheet, the index number of the worksheet
##' @param colNum If Excel sheet, the column number (A=1, B=2, etc.) containing the m/z values
##' @export
##' 

read_mz_file <- function(fn, sheetIndex=1, colNum=1) {
  
  # Define functions to read files depending on the file extension
  xlsx <- function(fn, sheetIndex, colNum) {
    df <- read.xlsx(fn=fn, sheetIndex=sheetIndex)
  }
  
  csv <- function(fn, colNum) {
    df <- read.csv(fn=fn)
  }
  
  # Determine whether file is .csv or .xlsx - lifted directly from ggsave
  save_fxn <- function(...) {
    pieces <- strsplit(fn, "\\.")[[1]]
    ext <- tolower(pieces[length(pieces)])
  }
  
  # Function to choose function to open files
  data_reader <- function(...) {
    pieces <- strsplit(fn, "\\.")[[1]]
    ext <- tolower(pieces[length(pieces)])
    match.fun(ext)
  }
  
  data_reader(fn, sheetIndex, colNum)
  
  #df <- data_reader(fn, sheetIndex, colNum)
  
  
  
}