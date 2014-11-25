##' Plot timecourses
##' 
plot_timecourse <- function(df, exp_var, x.var="time", y.var="relative.ion.count", exp_pred, print.plot=TRUE, save.plot=FALSE, fn, ...) {
  if(is.null(exp_var)) {
    p <- ggplot(df, aes_string(x=x.var, y=y.var)) + 
      geom_point() 
  } else { # Could include all kinds of cases where there are replicates, or whatver
    
    # remove replicate, time
    exp_var_short <- exp_var[-which(exp_var=="replicate" | exp_var=="time")]
    message("This will fail if there is not an exp_var caled \"replicate\" AND one called \"time\"")
    
    browser()
    
    # TESTING plots with Abigail's example
    p <- ggplot(df, aes_string(x=x.var, y=y.var)) + 
      geom_point() + 
      #geom_line(aes_string(group="replicate")) +
      facet_wrap(as.formula(paste("compound ~", do.call(paste, c(as.list(exp_var_short), sep="+")))))
                 
    
#     p <- ggplot(df, aes_string(x=x.var, y=y.var)) + 
#       geom_point() + 
#       geom_line(aes_string(group_by="replicate")) +
#       facet_wrap(as.formula(paste("~", do.call(paste, c(as.list(exp_var_short), sep="+"))))
  }
  
  if(!is.null(exp_pred)) {
    p <- p + 
      geom_line(data=exp_pred, aes_string(x=x.var, y=y.var, colour=treatment))
  }
  
  if(print.plot) {
    print(p)
  }
  
  if(save.plot) {
    ggsave(fn, p, ...)
  }
}