##' Identify the 12C peak for a compound and calculate its percentage vs all ions
##' 
calc_12C <- function(raw) {
  
  browser()
  # Add a column for the sum of all ion counts for that compound
  ### Damn that's slow - shold rewrite in dplyr
  raw_ion_sum <- ddply(raw, c("compoundId", "sample"), mutate, 
                       sum.ion.count=sum(ion.count, na.rm=TRUE), 
                       is.12C = medMz==min(medMz), 
                       relative.ion.count = ion.count/sum.ion.count)
  

  
}