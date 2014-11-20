##' Reads long data
##' 
read_long <- function(data.path) {
  #browser()
  # Determine all the .csv files in the relevant directory
  all_files <- paste(data.path, "/", dir(data.path, pattern=".csv$"), sep="")
  
  # Read all the .csv files in and put them into one giant data frame
  all_data <- adply(all_files, 1, read.csv)
  
  # the Replicate column should be a factor
  all_data$Replicate <- as.factor(all_data$Replicate)
  
  all_data
}