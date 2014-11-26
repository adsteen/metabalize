##' Generates expressions for facets
##' 
##' Right now used only by plot_timecourse.R
##' 
facet_expression <- function(exp_var_short) {
  
  facet_wrap(as.formula(paste(do.call(paste, c(as.list(exp_var_short), sep="+")))))

  
}