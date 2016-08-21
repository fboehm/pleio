#' Calculate likelihood at two loci for two (ordered) traits
#'
#' @param yy a n x 2 matrix containing two phenotypes
#' @param prob_mat1 a matrix of genotype (founder) probabilities
#' @param prob_mat2 a matrix of genotype (founder) probabilities
#' @export
calc_lik <- function(yy, prob_mat1, prob_mat2){
  res1 <- hk_residuals(pheno = yy[, 1], prob_mat1)
  res2 <- hk_residuals(pheno = yy[, 2], prob_mat2)
  res_mat <- cbind(res1, res2)
  return(det(t(res_mat) %*% res_mat))
}
