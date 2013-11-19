\name{toy_deisotope}
\alias{toy_deisotope}
\title{test deisotoping function}
\usage{
  toy_deisotope(d, mz_col = "medMz",
    names_col = "compound", mass_diff = 13.003355 - 12,
    tolerance = 2e-05)
}
\arguments{
  \item{d}{Data frame of MAVEN output}

  \item{mz_col,}{Vector of mz values (with no metadata)}

  \item{mass_diff}{Mass difference between two isotopes;
  default is C12-C13}

  \item{tolerance}{Tolerance for mass difference - function
  will identify peaks that differ in m/z by mass\_diff +/-
  tolerance}
}
\description{
  Test function to identify and remove isotopes from a
  vector of mz values
}

