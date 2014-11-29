plot_timecourse2 <- function(df, 
                            exp.var, 
                            x.var="time", 
                            y.var="relative.ion.count", 
                            treat.var="treatment", 
                            rep.var="replicate",
                            ignore.reps=TRUE,
                            exp_pred, 
                            print.plot=TRUE, save.plot=FALSE, fn, ...) {
  
  # Test for missing exp.var
  if(is.null(exp.var)) {
    stop("No vector of experimental variable(s) was provided. At minimum, exp.var must contain the variable which will be mapped to the x-axis (usually time)")
  }
  
  # exp.var must contain x.var (not sure if it makes sense to structure this this way, but I will for now)
  if (!(x.var %in% exp.var)) {
    stop(paste("exp.var, the vector of experimental variables, does not contain ", x.var, ", which is listed as the x variable.", sep=""))
  }
  
  #########
  # Shitty kluge
  #########
  # Remove "replicate" from exp.var preemptively
  if(ignore.reps) {
    exp.var <- exp.var[-which(exp.var %in% rep.var)]
  }
  
  # Convert replicate value into a factor
  if(treat.var %in% names(df)) { # No idea if this works
    df[ , treat.var] <- as.factor(df[ , treat.var])
  }
  
  
  # Remove replicate variable from exp.var, if it is in there
  exp.var.short <- exp.var[-which((exp.var %in% x.var) | (exp.var %in% treat.var))]
  
  # Possible cases:
  # IF generate_facet_statement returns NULL when appropriate, then there are only two cases:
  #   1. color by (something)
  #   2. don't color by anything
  # 1. exp.var contains nothing, after removing x.var and possibly rep.var if !distinguish
  #   i.e., user doesn't want you to facet by anything
  if(length(exp.var.short)) {
    print("Case 1")  
    p <- ggplot() + 
      geom_point(data=df, aes_string(x=x.var, y=y.var)) + 
      generate_facet_expression(exp.var.short)
  }
  
  # 2. Facetting is needed 
  if(length(exp.var.short)) {
    print("Case 2")
    p <- ggplot() + 
      geom_point(data=df, aes_string(x=x.var, y=y.var, colour=treat.var)) + 
      generate_facet_expression(exp.var.short, omit=treat.var)
  }
  
  ##############
  # Generate predictions for variables
  ##############
  
  if(!is.null(exp_pred)) {
    if(treat.var %in% names(exp_pred)) {
      p <- p + 
        geom_line(data=exp_pred, aes_string(x=x.var, y=y.var, colour=treat.var))
    } else {
      p <- p + 
        geom_line(data=exp_pred, aes_string(x=x.var, y=y.var))
    }
  }
  
  # Return the plot
  p
}