##' Read raw-ish MAVEN output
##' 
##' @param data_fn Filename (including path) of the data file
##' @param parse_col_names Not yet implemented
##' @param key_fn Filename (including path) of the key file
##' @param id.cols I can't remember what this does
##' @export
read_MAVEN <- function(data_fn, parse_col_names=FALSE, key_fn, id.cols=NULL) {
  d <- read.csv(data_fn)
  
  #browser()
  # Someday I would like to implement a column name parser, but I won't do this just yet
  if(parse_col_names) {
    stop("The functionality to parse column names automatically has not yet been implemented.")
  } else {
    if(is.null(key_fn)) {
      stop("You need to supply a filename for the sample key!")
    }
    key <- read.csv(key_fn)
  }
  #browser()
  # Determine experimental variables
  exp.var <- names(key)[2:length(names(key))] # experimental variables are everything in the sample key except the key name
  
  #KLUGE TO FIX:
  key$sample <- as.factor(paste("X", key$sample, sep=""))
  
  
 
  #
  # Assign default column names (note that id.cols is not really the right name since )
  if(is.null(id.cols)) {
    id.cols <- c("label", "metaGroupId", "groupId", "goodPeakCount", "medMz", 
                 "medRt", "maxQuality", "note", "compound",
                 "compoundId","expectedRtDiff", "ppmDiff","parent")
  }
  
  # Melt the data frame
  dm <- reshape2::melt(d, id.vars=id.cols, variable.name="sample", value.name="ion.count")
  
  # Remove the rows where everything is NA
  dm <- dm[!is.na(dm$ion.count), ]
  
  # Merge key with data
  d_merge <- merge(dm, key, by="sample") # There's a problem with the sample names in key
  
  list(raw_data=d_merge, exp.var=exp.var)

}