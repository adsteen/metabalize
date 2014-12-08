##' Generate guesses for A and k in exponential fit
##' 
##' Fits a linear least-squares fit to log-transformed exponential data
generate_exp_guess <- function(x, y) {
  
  # Strip down x and y to only values that you can take a log of
  bad.y <- (y <= 0) | (is.na(y)) | is.infinite(y)
  y <- y[!bad.y]
  x <- x[!bad.y]
  
  if(length(x) < 2) {
    return(NA)
  } 
  
  # Take log values
  log.y <- log(y)
  
  # Create linear model of log-transformed data
  ### MUST WRAP THIS IN TRY.CATCH
  #lin.mod <- lm(log.y ~ x)
  lin.mod <- tryCatch(
    lm(log.y ~ x),
    error=function(err) {
      warning("lm error")
    },
    warning=function(warn) {
      warning("warning")
    }
    )
  
  # y = A exp^(kx)
  # log(Y) = log(A) + kx
  # k = slope; A = exp(intercept)
  
  A_guess <- exp(coef(lin.mod)[1])
  k_guess <- coef(lin.mod)[2]
  
  
  # Return vector of guesses
  list("A" = A_guess, "k" = k_guess)
  
}