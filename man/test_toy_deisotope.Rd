\name{test_toy_deisotope}
\alias{test_toy_deisotope}
\title{Tests time scaling of toy_deisotope function}
\usage{
  test_toy_deisotope(d,
    subset_lengths = c(10, 100, 500, 1000, 2000, 3000, 3500, 4000))
}
\arguments{
  \item{d}{A data frame (as output by read_mz_file) to work
  on}

  \item{subset_lengths}{A vector of lengths by which to
  subset the data frame}
}
\value{
  A matrix of numeric output from system.time The third
  column, "elapsed", is the wall-clock time from start to
  finish.
}
\description{
  Tests time scaling of toy_deisotope function
}

