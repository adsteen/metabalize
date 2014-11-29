##' Generates expressions for facets
##' 
##' Right now used only by plot_timecourse.R
##' 
##' 
# Could include an option to generate facet_grid if a vertical variable is given
generate_facet_formula <- function(exp.var.short, vert.var="compound", omit, facet="grid") {
  
  # Drop variables from exp.var.short
  if(!is.null(omit)) {
    # Test that omit is a character vector
    if(!is.character(omit) | is.vector(omit)) {
      error("The variable passed as omit is not a character vector")
    }
    # Drop variables from exp.var.short
    exp.var.short <- exp.var.short[-which(omit %in% exp.var.short)]
  }
  
  # Case 1: Exp.var.short is empty
  
  if(length(exp.var.short) == 0) {
    return(NULL)
  }
  
  if(length(exp.var.short) == 1) {
    form_text(paste("~", exp.var.short))
    grid <- FALSE
  }
  
  if(length(exp.var.short) == 2 ) {
    exp.var.shorter <- exp.var.short[-which(vert.var %in% exp.var.short)]
    form_text <- paste(vert.var, "~", do.call(paste, c(as.list(exp_var_short), sep="+")))
  }
  
  # Decide whether to return facet_grid or facet_wrap
  #   Should really be a switch statement
  if(facet=="grid") {
    # Return facet_grid
    facet_grid(as.formula(form_text)) # Could make a decision about whether to return facet_grid or facet_wrap
  } else {
    facet_wrap(as.formula(form_text))
  }
  
}

