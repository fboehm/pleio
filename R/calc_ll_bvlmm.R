#' Calculate log-likelihood for a bivariate LMM
#' 
#' @param yvec a vector of phenotype values, after removing NA values
#' @param Xmat a design matrix with needed number of rows
#' @param betahat estimate of beta; a vector of length 16
#' @param var a covariance matrix with needed dimensions
#' @export

calc_ll_bvlmm <- function(yvec, Xmat, betahat, var){
  length(yvec) -> n
  c_term <- - n * log(2 * pi) / 2
  logdet_term <- - log(det(var)) / 2
  res_term <- - t(yvec - Xmat %*% betahat) %*% solve(var) %*% (yvec - Xmat %*% betahat) / 2
  return(c_term + logdet_term + res_term)
}

