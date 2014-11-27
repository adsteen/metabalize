##' Generate guesses for A and k in exponential fit
##' 
##' Fits a linear least-squares fit to log-transformed exponential data
generate_exp_guess <- function(x, y) {
  
  # Log transform the y data
  log.y <- log(y)
  
  # Create linear model of log-transformed data
  ### MUST WRAP THIS IN TRY.CATCH
  lin.mod <- lm(log.y ~ x)
  
  # y = A exp^(kx)
  # log(Y) = log(A) + kx
  # k = slope; A = exp(intercept)
  
  A_guess <- exp(coef(lin.mod)[1])
  k_guess <- coef(lin.mod)[2]
  
  
  # Return vector of guesses
  list("A" = A_guess, "k" = k_guess)
  
}