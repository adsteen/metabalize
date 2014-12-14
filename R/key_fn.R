##' Function to read sample key
##' @description Right now just simply wraps read.csv
##' @param key_fn Filename of the key
##' @export
read_key <- function(key_fn, ...) {
  read.csv(key_fn, ...) #
}