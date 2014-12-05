##' Plot timecourses
##' Contains a nested if statement that is FROM THE DEVIL
plot_timecourseOLD <- function(df, 
                            exp.var, 
                            x.var="time", 
                            y.var="relative.ion.count", 
                            treat.var="treatment", 
                            rep.var="replicate",
                            distinguish.reps=FALSE,
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
  if(!distinguish.reps) {
    exp.var <- exp.var[-which(exp.var %in% rep.var)]
  }
  
browser()
#   # Convert replicate variable into a factor
  df[ , treat.var] <- as.factor(df[ , treat.var])

  # Remove replicate variable from exp.var, if it is in there
  # Possible cases:
  # # NOTE: exp.var MUST exist, because this only works if there is an x variable with which to plot
  # exp.var.short <- exp.var[-which(exp.var %in% x.var)]
exp.var.short <- exp.var[-which((exp.var %in% x.var) | (exp.var %in% treat.var))]


# #   df_summ <- ddply(df, exp.var-but-not-replicate, summarise, 
# #                    mean.y.var=mean(y.var, na.rm=TRUE),
# #                    sd.y.var=sd(y.var), na.rm=TRUE)
#   
  # 1. exp.var contains only x.var (seems unlikely; possible for tuneup experiments I guess)
if(length(exp.var)==1) { # Note that ethe presence of x.var in exp.var has already been tested
  print("Case 1")  
  p <- ggplot() + 
      geom_point(data=df, aes_string(x=x.var, y=y.var))
  }
 
  # 2. exp.var contains only x.var and replicate
  if(length(exp.var)==2 & sum(c(x.var, treat.var) %in% exp.var) == 2) { # There's got to be a better way to test that exp.var consists of only x.var and treat.var
    print("Case 2")
    exp.var.short <- exp.var.short[-which(exp.var %in% treat.var)]
    p <- ggplot() + 
      geom_point(data=df, aes_string(x=x.var, y=y.var, colour=replicate)) 
  }
  
  # 3. exp.var contains only x.var and one or more other value, but not treatment
  if((length(exp.var) >= 2) & !(treat.var %in% exp.var)) {
    print("case 3")
    p <- ggplot() + 
      geom_point(data=df, aes_string(x=x.var, y=y.var)) + 
      generate_facet_formula(exp.var.short)
  }
  
  # 4. exp.var contains x.var and replicate and one or more other values
if((length(exp.var) > 2) & (treat.var %in% exp.var)) {
  browser()
  print("Case 4")
  # Remove replicate from exp.var.short so it doesn't show up in the faceting
  exp.var.short <- exp.var.short[-which(exp.var.short %in% treat.var)]
    p <- ggplot() + 
      geom_point(data=df, aes_string(x=x.var, y=y.var, colour=treat.var)) + 
      generate_facet_formula(exp.var.short)
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