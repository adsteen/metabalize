##' Make basic timeseries plot
##' 
##' Function to make standardized plot of timeseries
make_basic_timeseries_plot <- function(df, save.plot=TRUE) {
  p <- ggplot(df, aes(x=Time, y=percent.12C, colour=Replicate)) + 
    geom_point() +
    geom_line() +
    xlab("incubation time") +
    ylab(expression(paste("percent ", phantom(0)^12, "C"))) +
    expand_limits(ymin=0) + # Makes y axis go down to 0
    facet_wrap(~sample.type)
  
  # CReate a filename with which to save the plot
  metabolite <- df[1, "metabolite"]
  fn.path=""
  fn <- paste(fn.path, metabolite, ".png", sep="")
  
  if(save.plot) {
    ggsave(fn, p, height=4, width=6, units="in", dpi=200)
  }
  
  p
}