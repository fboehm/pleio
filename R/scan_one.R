#' Calculate RSS for a single locus and a single trait
#' 
#' @param y a vector of phenotype values, length n
#' @param genomat a single genotype matrix, n x 8
#' @export
calc_rss_one <- function(y, genomat){
  res <- y - genomat %*% (solve(t(genomat) %*% genomat)) %*% t(genomat) %*% y
  return(sum(res^2))
}


#' One-dimensional scan over a region of interest using only intercept and genotypes
#' 
#' @param y a vector of n phenotype values 
#' @param genoarray a three-dimensional array of genotypes, like `probs$probs$`1``
#' @param start index for start of scan
#' @param stop index for stop of scan
#' @export
scan_one <- function(y, genoarray, start , stop){
  n <- length(y)
  rss <- vector(length = stop - start + 1)
  for (i in start:stop){
    rss[i - start + 1] <- calc_rss_one(y, genoarray[ , , i])
  }
  return(rss / n)
}