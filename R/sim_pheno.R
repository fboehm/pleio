#' Simulate phenotypes from genotype data
#'
#' @param genomat a matrix of genotype probabilities
#' @param beta a vector (length 8) of genotype class effects
#' @param sd standard deviation of simulated noise
#' @export
#'
sim_pheno <- function(genomat, beta = 1:8 / 8, sd = 0.05){
  pheno_length <- nrow(genomat)
  noise <- stats::rnorm(n = pheno_length, mean = 0, sd = sd)
  pheno <- genomat %*% beta + noise
  return(pheno)
}
