##' Plots timecourses
##' 
##' @param df data frame containing percent C12 data
##' @param exp.var character vector of experimental variables
##' @param x.var The x variable for the plot, usually time
##' @param yvar The y variable for the plot, usually relative ion count
##' @param rep.var The variable encoding replicates
##' @param color.by The variable by which to color the data. Usually treatment
##' @param ingnore.reps Whether to ignore replicates as a vairable with which to separate the data
##' @param exp_pred Data frame of exponential fit lines
##' @param print.plot CURRENTLY NOT IMPLEMENTED Whether to print the plot to screen. May not work outside of Rstudio
##' @param save.plot CURRENTLY NOT IMPLEMENTED Whether to save the plot.
##' @param plot.fn CURRENTLY NOT IMPLEMENTED Filename with which to save the plot
##' @export

plot_timecourse <- function(df, 
                            exp.var, 
                            x.var="time", 
                            y.var="relative.ion.count", 
                            #treat.var="treatment", 
                            rep.var="replicate",
                            color.by="sample.type", #in general this should be "treatment"
                            ignore.reps=TRUE,
                            exp_pred=NULL, 
                            print.plot=TRUE, save.plot=FALSE, plot.fn, ...) {
  ###########
  # Test parameters
  ###########
  
  stopifnot(is.data.frame(df))
  
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
  
#   # Convert replicate value into a factor
#   if(treat.var %in% names(df)) { # No idea if this works
#     df[ , treat.var] <- as.factor(df[ , treat.var])
#   }
  
  
  # Remove replicate variable from exp.var, if it is in there
  #exp.var.short <- exp.var[-which((exp.var %in% x.var) | (exp.var %in% treat.var))]
  exp.var.short <- exp.var[-which(exp.var %in% x.var)]
  #browser()
  # Possible cases:
  # IF generate_facet_statement returns NULL when appropriate, then there are only two cases:
  #   1. color by (something)
  #   2. don't color by anything
  # 1. exp.var contains nothing, after removing x.var and possibly rep.var if !distinguish
  #   i.e., user doesn't want you to facet by anything
  #if(length(exp.var.short==0)) {
  if(is.null(color.by)) {
    print("Case 1")  
    p <- ggplot2::ggplot() + 
      ggplot2::geom_point(data=df, aes_string(x=x.var, y=y.var)) + 
      ggplot2::generate_facet_formula(exp.var.short)
  } else {
    #browser()
    print(paste("Case 2: facetting by ", color.by))
    p <- ggplot2::ggplot() + 
      ggplot2::geom_point(data=df, aes_string(x=x.var, y=y.var, colour=color.by)) + 
      generate_facet_formula(exp.var.short, omit=rep.var)
  }
  
  
  ##############
  # Generate predictions for variables
  ##############
  
# Need to change this to remove reference to treat.var; switch to color.by
  if(!is.null(exp_pred)) {
    if(color.by %in% names(exp_pred)) {
      p <- p + 
        ggplot2::geom_line(data=exp_pred, aes_string(x=x.var, y=y.var, colour=color.by))
    } else {
      p <- p + 
        ggplot2::geom_line(data=exp_pred, aes_string(x=x.var, y=y.var))
    }
  }


  # Bring y axis down to zero
  p <- p + ggplot2::expand_limits(y=0)
  


  # Return the plot
  p
}