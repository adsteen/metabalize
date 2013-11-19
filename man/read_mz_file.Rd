\name{read_mz_file}
\alias{read_mz_file}
\title{Reads MAVEN output as .csv or .xlsx file}
\usage{
  read_mz_file(fn = "data/maven-output.csv",
    sheetIndex = 1, col = "medMz")
}
\arguments{
  \item{fn}{Filename (including path if necessary)}

  \item{sheetIndex}{If Excel sheet, the index number of the
  worksheet}
}
\value{
  A data frame of whatever
}
\description{
  Reads MAVEN output. File can be .xlsx or .csv, must have
  a column containing mz values
}

