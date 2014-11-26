##' Plot timecourses
##' Contains a nested if statement that is FROM THE DEVIL
plot_timecourse <- function(df, 
                            exp_var, 
                            x.var="time", 
                            y.var="relative.ion.count", 
                            rep.var="replicate", 
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
  
  # Possible cases:
  # # NOTE: exp_var MUST exist, because this only works if there is an x variable with which to plot
  exp_var_short <- exp_var[-which(exp_var %in% x.var)]
  df_summ <- ddply(df, exp_var-but-not-replicate, summarise, 
                   mean.y.var=mean(y.var, na.rm=TRUE),
                   sd.y.var=sd(y.var), na.rm=TRUE)
  # 1. exp_var contains only x.var (seems unlikely; possible for tuneup experiments I guess)
  p <- ggplot(df, aes(x=x.var, y=y.var)) + 
    geom_point()
  # 2. exp_var contains only x.var and replicate
  exp_var_short <- exp_var[-which(exp_var %in% rep.var)]
  
  p <- ggplot(df, aes(x=x.var, y=y.var, colour=replicate)) + 
    geom_point() 
  # 3. exp_var contains only x.var and one or more other value, but not replicate
  p <- ggplot(df, aes(x=x.var, y=y.var)) + 
    geom_point() + 
    facet_wrap(~[all the exp.vars except time])
  
  # 4. exp_var contains x.var and replicate and one or more other values
  p <- ggplot(df, aes(x=x.var, y=y.var, colour=rep.var)) + 
    geom_point() + 
    facet_wrap(![all the exp vars except replicate and tiem])
  
#   # Case when there is no experimental variable at all. THIS SHOULD BE RARE
#   if(is.null(exp_var)) {
#     p <- ggplot(df, aes_string(x=x.var, y=y.var)) + 
#       geom_point() 
#   } else { # There exp_var has at least one element
#     # Case in which there is only replicate (rep.var) in the experimental treatment
#     if("rep.var" %in% exp_var) {
#       p <- ggplot(df, aes_string(x=x.var, y=y.var, colour=rep.var)) +
#         geom_point() 
#       # Calculate average of replicates
# #       df_summ <- ddply(df, c(SOMETHING), summarise,
# #                        mean.y=mean(y.var))
# #       p <- p+geom_line(data=df_summ, aes_string(x=x.var, y=y.var))
#     } else {
#       # remove replicate & time variables from exp_var
#       exp_var_short <- exp_var[-which(exp_var==rep.var | exp_var==x.var)]
#       
#       # TESTING plots with Abigail's example
#       p <- ggplot(df, aes_string(x=x.var, y=y.var, colour=rep.var)) + 
#         geom_point() + 
#         #geom_line(aes_string(group="replicate")) +
#         facet_wrap(as.formula(paste("compound ~", do.call(paste, c(as.list(exp_var_short), sep="+")))))
#       
#     }
#     
#  }
#   
#   if(!is.null(exp_pred)) {
#     p <- p + 
#       geom_line(data=exp_pred, aes_string(x=x.var, y=y.var, colour=treatment))
#   }
#   
#   if(print.plot) {
#     print(p)
#   }
#   
#   if(save.plot) {
#     ggsave(fn, p, ...)
#   }
}