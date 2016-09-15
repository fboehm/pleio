#' Calculate determinant of 'squared residuals' (ie, RSS) at two loci for two (ordered) traits
#'
#' @param yy a n x 2 matrix containing two phenotypes
#' @param mat1 a matrix of genotype (founder) probabilities with first column being all 1's
#' @param mat2 a matrix of genotype (founder) probabilities with first column being all 1's
#' @export
calc_rss <- function(yy, mat1, mat2){
  res1 <- hk_residuals(pheno = yy[, 1], mat1)
  res2 <- hk_residuals(pheno = yy[, 2], mat2)
  res_mat <- cbind(res1, res2)
  return(det(t(res_mat) %*% res_mat))
}