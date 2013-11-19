##' Tests time scaling of toy_deisotope function
##' 
##' 
##' @param d A data frame (as output by read_mz_file) to work on
##' @param subset_lengths A vector of lengths by which to subset the data frame
##' @return A matrix of numeric output from system.time The third column, "elapsed", is the wall-clock time from start to finish.
##' @export

test_toy_deisotope <- function(d, subset_lengths=c(10, 100, 500, 1000, 2000, 3000, 3500, 4000)) {
  time_mat <- matrix(nrow=length(subset_lengths), ncol=3)

  colnames(time_mat) <- c("user", "system", "elapsed")
  for (i in 1:length(subset_lengths)) {
    subs <- d[1:subset_lengths[i], ]
    time_vec <- as.numeric(system.time(is <- toy_deisotope(subs)))[1:3]
    time_mat[i, ] <- time_vec
    #print("for vector length ", subset_vec[i], " system.time is")
    print(time_vec)
    
    # Collect garbage
    gc()
  }
  
  time_mat <- cbind(subset_lengths, time_mat)
  as.data.frame(time_mat)
  
}
