#' Calculate residuals with Haley-Knott regression
#'
#' @param pheno a phenotype vector or matrix (with each column being one phenotype)
#' @param genomat a matrix of genotype probabilities, with the first column of probabilities replaced with a column of 1's.
#' @export
#'
hk_residuals <- function(pheno, genomat){
  residuals(lm(pheno ~ genomat))
}
