#' Calculate RSS for a single locus and a single trait
#' 
#' @param y a vector of phenotype values, length n
#' @param genomat a single genotype matrix, n x 8
#' @export
calc_rss_one <- function(y, genomat){
  foo <- lm(y ~ genomat) 
  res <- residuals(foo)
  return(sum(res^2))
}



#' One-dimensional scan over a region of interest using only intercept and genotypes
#' 
#' @param y a vector of n phenotype values 
#' @param genoarray a three-dimensional array of genotypes, like `probs$probs$`1``
#' @export
calc_rss <- function(y, genoarray){
  k <- dim(genoarray)[3]
  rss <- vector(length = k)
  for (i in 1:k){
    rss[i] <- calc_rss_one(y, genoarray[ , , i])
  }
  return(rss)
}