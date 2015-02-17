##' Full flux analysis
##' 
##' @param data.fn Filename of raw data file 
##' @param key.fn Filename of the sample key (see vignette for information on what this is)
##' @param vars.to.fit.by Varibles that define a unique fit. For example, if you want each replicate to get its own unique fit, 
##' @export
##' 
fit_raw <- function(data.fn, key.fn, vars.to.fit.by, ...) {
  
  # Can pull extra params by name out of extra_params
  extra_params <- list(...)
  
  raw_list <- read_MAVEN(data.fn=data.fn,
                         key_fn=key_fn)
  
  # Separate the raw data from the experimental variables
  raw <- raw_list_ac$raw_data
  exp.var <- raw_list_ac$exp.var
  
  # Identify unlabeled peaks
  C12 <- calc_12C(raw)
  
  # Strip out labeled peaks
  C12_only <- subset(ac_12C, is.12C==TRUE)
  
  # Calculate nls fits
  nls_fits <- dlply(C12_only, 
                    vars.to.fit.by, safe_NLS, .inform=TRUE)
  
  # Pull out the nls coefficients
  nls_coefs <- ldply(nls_fits_ac, function(x) exp_coef(x))
  
  # Name the coefs vec
  names(nls_coefs)[5:9] <- c("A.est", "k.est", "A.std.err", "k.std.err", "n")
  
  # Summarize average k values
  nls_fits_summary <- ddply(ac_coefs, c("compound", "treatment", "sample.type"), summarise,
                           mean.k = mean(k.est, na.rm=TRUE),
                           sd.k=sd(k.est, na.rm=TRUE),
                           mean.A = mean(A.est, na.rm=TRUE),
                           sd.A=sd(A.est, na.rm=TRUE))
  
  # Autoprint summarized data frame
  nls_fits_summary
}