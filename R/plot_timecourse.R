##' Plot timecourses
##' Contains a nested if statement that is FROM THE DEVIL
plot_timecourse <- function(df, 
                            exp_var, 
                            x.var="time", 
                            y.var="relative.ion.count", 
                            treat.var="treatment", 
                            exp_pred, 
                            print.plot=TRUE, save.plot=FALSE, fn, ...) {
  
  # Test for missing exp_var
  if(is.null(exp_var)) {
    stop("No vector of experimental variable(s) was provided. At minimum, exp_var must contain the variable which will be mapped to the x-axis (usually time)")
  }
  
  # exp_var must contain x.var (not sure if it makes sense to structure this this way, but I will for now)
  if (!(x.var %in% exp_var)) {
    stop(paste("exp_var, the vector of experimental variables, does not contain ", x.var, ", which is listed as the x variable.", sep=""))
  }
  
#   browser()
#   # Convert replicate variable into a factor
  df[ , treat.var] <- as.factor(df[ , treat.var])
#   
#   
#   
  # Remove replicate variable from exp_var, if it is in there
  # Possible cases:
  # # NOTE: exp_var MUST exist, because this only works if there is an x variable with which to plot
  exp_var_short <- exp_var[-which(exp_var %in% x.var)]

# #   df_summ <- ddply(df, exp_var-but-not-replicate, summarise, 
# #                    mean.y.var=mean(y.var, na.rm=TRUE),
# #                    sd.y.var=sd(y.var), na.rm=TRUE)
#   
  # 1. exp_var contains only x.var (seems unlikely; possible for tuneup experiments I guess)
if(length(exp_var)==1) { # Note that ethe presence of x.var in exp_var has already been tested
    p <- ggplot() + 
      geom_point(data=df, aes_string(x=x.var, y=y.var))
  }
 
  # 2. exp_var contains only x.var and replicate
  if(length(exp_var)==2 & sum(c(x.var, treat.var) %in% exp_var) == 2) { # There's got to be a better way to test that exp_var consists of only x.var and treat.var
    exp_var_short <- exp_var_short[-which(exp_var %in% treat.var)]
    p <- ggplot() + 
      geom_point(data=df, aes_string(x=x.var, y=y.var, colour=replicate)) 
  }
  
  # 3. exp_var contains only x.var and one or more other value, but not replicate
  if(length(exp_var) >= 2 & !(treat.var %in% exp_var)) {
    p <- ggplot() + 
      geom_point(data=df, aes_string(x=x.var, y=y.var)) + 
      generate_facet_formula(exp_var_short)
  }
  
  # 4. exp_var contains x.var and replicate and one or more other values
if(length(exp_var > 2) & treat.var %in% exp_var) {
  # Remove replicate from exp_var_short so it doesn't show up in the faceting
  exp_var_short <- exp_var_short[-which(exp_var_short %in% treat.var)]
    p <- ggplot() + 
      geom_point(data=df, aes_string(x=x.var, y=y.var, colour=treat.var)) + 
      generate_facet_formula(exp_var_short)
  }

########
# Generate predictions for values
########
if(!is.null(exp_pred)) {
  # browser()
  # Gotta separate out case where treat.var is in the exponential predictions, and isn't
  if(treat.var %in% names(exp_pred)) {
    p_new <- p +
      geom_line(data=exp_pred, aes_string(x=x.var, y=y.var, colour=treat.var))
  } else {
    p_new <- p +
      geom_line(data=exp_pred, aes_string(x=x.var, y=y.var))
  }
  p <- p_new # When this is up and running, change all p_new to p
}
  p
}