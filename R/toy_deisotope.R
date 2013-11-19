##' test deisotoping function
##' 
##' @description Test function to identify and remove isotopes from a vector of mz values
##' @param d Data frame of MAVEN output
##' @param mz_col, Vector of mz values (with no metadata)
##' @param mass_diff Mass difference between two isotopes; default is C12-C13
##' @param tolerance Tolerance for mass difference - function will identify peaks that differ in m/z by mass\_diff +/- tolerance
##' @export

toy_deisotope <- function(d, mz_col="medMz", names_col="compound", mass_diff=13.003355-12, tolerance=2e-5) {
  
  # Pull out the mz vector 
  mz <- d[ , mz_col]
  names(mz) <- d[, names_col]
  rm(d)
  
  # Check whether the column is numeric - there is probably a stricter way to do this (this way will fail with NAs, etc.)
  if(!is.numeric(mz)) {
    stop(paste("mz_col", mz_col, "is not numeric."))
  }
  
  # Calculate difference between each pair of mz's in the matrix
  mz.grid <- expand.grid(mz, mz) #combn might be an alternative to expand.grid
  diffs <- mz.grid[ , 2] - mz.grid[ , 1]
  colnames(mz.grid) <- c("mz1", "mz2")
 
  # Pick out differences that are equal to isotope diff
  low_diff <- mass_diff - tolerance
  hi_diff <- mass_diff + tolerance
  
  isotopomers <- mz.grid[(diffs > (low_diff)) & (diffs < hi_diff), ] # This will be slow with large datasets
  
  isotopomers
  
}