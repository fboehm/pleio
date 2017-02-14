#' Calculate an array of residuals using Haley-Knott regression
#'
#' @param yy n x 2 data matrix
#' @param geno_array genotype probabilities array
#' @export

calc_res <- function(yy, geno_array){
  n_marker <- dim(geno_array)[3]
  res <- array(NA, c(nrow(yy), ncol(yy), n_marker))
  for (i in 1:n_marker){
    res[ , , i] <- hk_residuals(yy, geno_array[ , , i])
  }
  return(res)
}
