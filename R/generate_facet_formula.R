##' Generates expressions for facets
##' 
##' Right now used only by plot_timecourse.R
##' @param exp.var.short Character vector of the names of the experimental variables. Currently there is some ambiguity about whether the time variable should be included in exp.var
##' @param vert.var Variable used for the vertical component of the facet #Describe this more clearly
##' @param omit Variables to remove
##' @param facet Type of facetting. "grid" or "wrap". Only grid is currently implemented.
##' @export

generate_facet_formula <- function(exp.var.short, vert.var="compound", omit=NULL, facet="grid") {
  #browser()
#   # Drop variables from exp.var.short
#   if(!is.null(omit)) {
#     # Test that omit is a character vector
#     if(!is.character(omit) | !is.vector(omit)) {
#       stop("The variable passed as omit is not a character vector")
#     }
#     # Drop variables from exp.var.short
#     exp.var.short <- exp.var.short[-which(omit %in% exp.var.short)]
#   }
#   #browser()
#   # Case 1: Exp.var.short is empty. We still want to facet by vert.var, I believe
#   #browser()
#   if(length(exp.var.short) == 0) {
#     return.null <- TRUE
#   }
#   
#   if(length(exp.var.short) == 1) {
#     form_text <- paste("~", exp.var.short)
#     grid <- FALSE
#   }
#   
#   if(length(exp.var.short) == 2) {
#     exp.var.shorter <- exp.var.short[-which(vert.var %in% exp.var.short)]
#     form_text <- paste(vert.var, "~", do.call(paste, c(as.list(exp.var.short), sep="+")))
#   }
#   
#   # Decide whether to return facet_grid or facet_wrap
#   #   Should really be a switch statement
#   if(facet=="grid") {
#     # Return facet_grid
#     f <- facet_grid(as.formula(form_text)) # Could make a decision about whether to return facet_grid or facet_wrap
#   } else {
#     f <- facet_wrap(as.formula(form_text))
#   }
#   browser()
#   
#   # Trying to prevent multiple outcomes
#   if(return.null) { 
#     f <- NULL
#   }
  
  #######
  # KLUGE TO MAKE IT WORK FOR NOW
  #####
  right.side <- do.call(paste, c(as.list(exp.var.short), sep=" + "))
  #f <- ggplot2::facet_grid(as.formula("compound ~ treatment"))
  f <- ggplot2::facet_grid(as.formula(paste("compound ~", right.side)))
  
#f <- facet_wrap(as.formula("compound + treatent"))
}

