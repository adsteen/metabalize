##' Read raw-ish MAVEN output
##' 
read_MAVEN <- function(fn, id.cols=NULL) {
  d <- read.csv(fn)
  
  # Assign default column names (note that id.cols is not really the right name since )
  if(is.null(id.cols)) {
    id.cols <- c("label", "metaGroupId", "groupId", "goodPeakCount", "medMz", 
                 "medRt", "maxQuality", "note", "compound",
                 "compoundId","expectedRtDiff", "ppmDiff","parent")
  }
  
  # Melt the data frame
  dm <- melt(d, id.vars=id.cols, variable.name="ion.count", value.name="sample")
  
  dm
}