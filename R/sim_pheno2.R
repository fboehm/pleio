#' Simulate two phenotypes simultaneously
#'
#' @param genomat a matrix of genotypes
#' @param bmat a matrix of effect sizes
#' @param Sigma a 2x2 covariance matrix
#' @export

sim_pheno2 <- function(genomat, bmat, Sigma){
  n_mouse <- nrow(genomat)
  noise <- MASS::mvrnorm(n = n_mouse, mu = c(0, 0), Sigma = Sigma)
  pheno <- genomat %*% bmat + noise
  return(pheno)
}
