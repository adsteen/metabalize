\name{toy_deisotope}
\alias{toy_deisotope}
\title{test deisotoping function}
\usage{
  toy_deisotope(mz, isotope_diff = 1.003355 - 12,
    tolerance = 2e-05)
}
\arguments{
  \item{mz}{Vector of mz values (with no metadata)}

  \item{isotope_diff}{<- mass difference between two
  isotopes; default is C12-C13}
}
\description{
  Test function to identify and remove isotopes from a
  vector of mz values
}

