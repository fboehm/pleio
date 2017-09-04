#' Simulate bivariate phenotype with incorporation of random effects
#' 
#' @param genomat a single locus's genotypes probability matrix
#' @param bmat a matrix of effect sizes
#' @param Sigma a 2n by 2n covariance matrix
#' @examples 
#' Kmat <- diag(1, 8)
#' Vg <- matrix(c(1:4 / 10), nrow = 2, ncol = 2)
#' Ve <- matrix(c(2:5) / 10, nrow = 2, ncol = 2)
#' In <- diag(1, 8)
#' sim_pheno_lmm(genomat = matrix(c(1, 0, 0, 1, 2, 1, 0, 1), ncol = 1), 
#' bmat = matrix(c(2.5, 3), nrow = 1), 
#' Sigma = Vg %x% Kmat + Ve %x% In)
#' 
#' @export
sim_pheno_lmm <- function(genomat, bmat, Sigma){
  stopifnot(ncol(genomat) == nrow(bmat))
  as.vector(genomat %*% bmat) -> means
  n <- nrow(genomat)
  noise <- MASS::mvrnorm(n = 1, mu = rep(0, 2 * n), Sigma = Sigma)
  out <- matrix(data = means + noise, ncol = 2, byrow = FALSE)
  colnames(out) <- c("phenotype1", "phenotype2")
  rownames(out) <- rownames(genomat)
  return(out)
}