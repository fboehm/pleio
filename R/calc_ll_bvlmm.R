#' Calculate log-likelihood for a bivariate LMM
#' 
#' @param yvec a vector of phenotype values, after removing NA values
#' @param Xmat a design matrix with needed number of rows
#' @param betahat estimate of beta; a vector of length 16
#' @param Vg 2 by 2 Vg matrix
#' @param Ve 2 by 2 Ve matrix
#' @param Kmat kinship matrix, for appropriate chromosome
#' @export

calc_ll_bvlmm <- function(yvec, Xmat, betahat, Vg, Ve, Kmat){
  n <- nrow(Kmat)
  In <- diag(1, n)
  mvtnorm::dmvnorm(yvec, mean = Xmat %*% betahat, sigma = Vg %x% Kmat + Ve %x% In)
}

