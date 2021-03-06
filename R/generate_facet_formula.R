##' Generates expressions for facets
##' 
##' Right now used only by plot_timecourse.R
##' 
##' 
# Could include an option to generate facet_grid if a vertical variable is given
generate_facet_formula <- function(exp.var.short, vert.var="compound", omit=NULL, facet="grid") {
#   #browser()
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
  f <- facet_grid(as.formula("compound ~ treatment"))
}

