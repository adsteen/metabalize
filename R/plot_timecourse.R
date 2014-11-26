##' Plot timecourses
##' Contains a nested if statement that is FROM THE DEVIL
plot_timecourse <- function(df, 
                            exp_var, 
                            x.var="time", 
                            y.var="relative.ion.count", 
                            rep.var="replicate", 
                            exp_pred, 
                            print.plot=TRUE, save.plot=FALSE, fn, ...) {
  
  
  # Case when there is no experimental variable at all. THIS SHOULD BE RARE
  if(is.null(exp_var)) {
    p <- ggplot(df, aes_string(x=x.var, y=y.var)) + 
      geom_point() 
  } else { # There exp_var has at least one element
    # Case in which there is only replicate (rep.var) in the experimental treatment
    if("rep.var" %in% exp_var) {
      p <- ggplot(df, aes_string(x=x.var, y=y.var, colour=rep.var)) +
        geom_point() 
      # Calculate average of replicates
#       df_summ <- ddply(df, c(SOMETHING), summarise,
#                        mean.y=mean(y.var))
#       p <- p+geom_line(data=df_summ, aes_string(x=x.var, y=y.var))
    } else {
      # remove replicate & time variables from exp_var
      exp_var_short <- exp_var[-which(exp_var==rep.var | exp_var==x.var)]
      
      # TESTING plots with Abigail's example
      p <- ggplot(df, aes_string(x=x.var, y=y.var, colour=rep.var)) + 
        geom_point() + 
        #geom_line(aes_string(group="replicate")) +
        facet_wrap(as.formula(paste("compound ~", do.call(paste, c(as.list(exp_var_short), sep="+")))))
      
    }
    
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