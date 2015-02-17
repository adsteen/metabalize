##' Read raw-ish MAVEN output
##' 
##' @param data.fn Filename (including path) of the data file
##' @param parse_col_names Not yet implemented
##' @param key.fn Filename (including path) of the key file
##' @param id.cols Character vector containing all of the "id.columns" that come with xcms output
##' @param numeric.sample.names If sample names begin with numbers, this must be set TRUE. If SOME but not ALL sample names are numeric, this will fail
##' @param rep.name Name of the "replicate" variable in the sample key, if it exists
##' @export
read_MAVEN <- function(data.fn, 
                       parse_col_names=FALSE, 
                       key.fn, id.cols=c("label", "metaGroupId", "groupId", "goodPeakCount", "medMz", 
                                                                         "medRt", "maxQuality", "note", "compound",
                                                                         "compoundId","expectedRtDiff", "ppmDiff","parent"),
                       numeric.sample.names=TRUE,
                       rep.name="replicate") {

  # Read the raw data
  d <- read.csv(data.fn)
  
  # Someday I would like to implement a column name parser, but I won't do this just yet
  # Read the sample key
  if(parse_col_names) {
    stop("The functionality to parse column names automatically has not yet been implemented.")
  } else {
    if(is.null(key.fn)) {
      stop("You need to supply a filename for the sample key!")
    }
    key <- read.csv(key.fn)
  }
  
  # Determine experimental variables
  exp.var <- names(key)[2:length(names(key))] # experimental variables are everything in the sample key except the key name
  
  # If sample names in the raw data file are numeric, they will automatically be appended with X
  # Could use regular expressions to append X only to key sample names that start with numbers
  if(numeric.sample.names) {
    key$sample <- as.factor(paste("X", key$sample, sep=""))
  }
  
  # Check for mismatch between key names and sample names
  if(sum(unique(d$sample) %in% unique(key$sample)) < length(unique(d$sample))) {
    warning("Some samples listed in the raw dataset are not listed in the sample key. /n
            What's worse, no one has implemented code to tell you _which_ samples are missing from the key, even though it would be easy to do so.")
  }
  
  
  # Melt the data frame
  dm <- reshape2::melt(d, id.vars=id.cols, variable.name="sample", value.name="ion.count")
  
  # Remove the rows where everything is NA
  dm <- dm[!is.na(dm$ion.count), ]

  # Merge key with data
  d_merge <- merge(dm, key, by="sample") 
  
  # Set replicate value to factor
  if(rep.name %in% exp.var) {
    d_merge[ , rep.name] <- as.factor(d_merge[ , rep.name])
  }

  list(raw_data=d_merge, exp.var=exp.var)
}