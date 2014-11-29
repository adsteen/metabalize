# Calculate NLS & return
# To do: import model as a parameter

safe_NLS <- function(df, xvar="time", yvar="relative.ion.count") {
  
  form <- formula(I(relative.ion.count ~ A * exp(k*time)))
  
  
  ########
  # Test arguments
  ########
  
  if(is.data.frame(df)) {
    # Test for the presence of xvar and yvar in df
    if(!(xvar %in% names(df))) {
      stop(paste("There is no column in df called ", xvar))
    }
    if(!(yvar %in% names(df))) {
      stop(paste("There is no column in df called ", yvar))
    }
  } else {
    stop("df must be a data frame, but the object you have passed is something else.")
  }
  
  #browser()
  
  # Turn xvar and yvar into vectors
  xvals <- df[ , xvar]
  yvals <- df[ , yvar]
  
  # Generate guesses for exponential fits
  guesses <- generate_exp_guess(xvals, yvals)
  
  # Determine domain for predictions
  dom <- c(min(xvals), max(xvals))
  
  mod <- tryCatch({
    mod <- nls(form, df, start=guesses)
    #pred <- safe_NLS_pred(mod, domain=dom)
    #list(mod=mod, pred=pred)
    },
    # Note: on warning, the function executes and the warning is issued
    error=function(err) {
      warning("This model threw an error")
      NULL
      })
  
  mod
}

