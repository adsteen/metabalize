##' Generates expressions for facets
##' 
##' Right now used only by plot_timecourse.R
##' 
##' 
# Could include an option to generate facet_grid if a vertical variable is given
generate_facet_formula <- function(exp_var_short, vert.var="compound") {
  form_text <- paste(vert.var, "~", do.call(paste, c(as.list(exp_var_short), sep="+")))
  facet_grid(as.formula(form_text))
}
#ggplot(mtcars, aes(x=disp, y=mpg)) + 
#  geom_point() +
  generate_facet_formula("carb")
