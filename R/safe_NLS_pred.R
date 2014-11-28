##' Generates nls predictions
##' 
##' @param mod is a nls model (or any object for which there is a predict method)
##' @param domain of the data, as a two-element vector
##' 
safe_NLS_pred <- function(mod, domain) {
  
  
  # Determine the domain over which to predict the nls object
  domain <- seq(from=min(domain), to=max(domain), length.out=100)
  
  # Put the domain into an APPROPRIATELY NAMED data frame
  pred_df <- data.frame(time=domain)
  # This gets me the name of the x variable but I don't know how to use it
  # names(mod$dataClasses)[1]
  
  # Predict the formula over the grid
  pred.vec <- predict(mod, newdata=pred_df)
  pred.df <- data.frame(time=domain, relative.ion.count=pred.vec)
  
  pred.df
}