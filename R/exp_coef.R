##' Returns parameters of an exponential model
##' @description Right now only works with two-factor exponential models
##' @param mod An nls model. Right now should be an exponential model of the form y=Ae^(kx)
##' @export
exp_coef <- function(mod) {
  #browser()
  # Check whether model is an nls model
  if(class(mod) != "nls") {
    print("I rejected this case")
    return(rep(NA, 5))
  }
  
  # Get the coefficients
  coefs <- summary(mod)$coefficients[ , 1:2]
  coefs.vec <- as.vector(coefs)
  
  # Test whether there are more than two rows: this could be trouble
  if(nrow(coefs) != 2) {
    rep(NA, 5)
    warning("This model did not have exactly two fitted terms")
  }
  
  # Get the number of points
  n <- length(resid(mod))
  coefs.vec[5] <- n
  
  # Name the vector
  rn <- rownames(coefs)
  coefs.names <- c(paste(rn[1], ".est", sep=""),
                   paste(rn[2], ".est", sep=""),
                   paste(rn[1], "std.err", sep=""), 
                   paste(rn[2], "std.err", sep=""),
                   "n")
  names(coefs.vec) <- coefs.names
  
  coefs.vec
}