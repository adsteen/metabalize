##' test deisotoping function
##' 
##' @description Test function to identify and remove isotopes from a vector of mz values
##' @param mz Vector of mz values (with no metadata)
##' @param isotope_diff <- mass difference between two isotopes; default is C12-C13
##' @export

toy_deisotope <- function(mz, isotope_diff=1.003355-12, tolerance=2e-5) {
  
  # Calculate difference between each pair of mz's in the matrix
  mz.grid <- expand.grid(mz_unknown, mz_unknown) #combn might be an alternative to expand.grid
  diffs <- mz.grid[ , 2] - mz.grid[ , 1]
 
  # Pick out differences that are equal to isotope diff
  low_diff <- isotope_diff - tolerance
  hi_diff <- isotope_diff + tolerance
  isotopomers <- mz.grid[(diffs > (low_diff)) & (diffs < hi_diff)] # This will be slow with large datasets
  
  isotopomers
  
}