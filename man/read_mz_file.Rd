\name{read_mz_file}
\alias{read_mz_file}
\title{Reads MAVEN output as .csv or .xlsx file}
\usage{
  read_mz_file(fn, sheetIndex = 1, colNum = 1)
}
\arguments{
  \item{fn}{Filename (including path if necessary)}

  \item{sheetIndex}{If Excel sheet, the index number of the
  worksheet}

  \item{colNum}{If Excel sheet, the column number (A=1,
  B=2, etc.) containing the m/z values}
}
\description{
  Reads MAVEN output. File can be .xlsx or .csv, must have
  a column containing mz values
}

