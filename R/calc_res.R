#' Calculate an array of residuals using Haley-Knott regression
#'
#' @param yy n x 2 data matrix
#' @param geno_array genotype probabilities array
#' @param start marker index for scan start
#' @param stop marker index for scan stop
#' @export

calc_res <- function(yy, geno_array, start, stop){
  n_marker <- stop - start + 1
  res <- array(NA, c(nrow(yy), ncol(yy), n_marker))
  for (i in 1:n_marker){
    res[ , , i] <- hk_residuals(yy, cbind(1, geno_array[ , -1, start + i - 1]))
  }
  return(res)
}
