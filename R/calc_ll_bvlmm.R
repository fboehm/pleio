#' Calculate log-likelihood for a LMM
#' 
#' @param yvec a vector of phenotype values
#' @param Xmat a design matrix
#' @param betahat estimate of beta
#' @param var a covariance matrix
#' @export

calc_ll_bvlmm <- function(yvec, Xmat, betahat, var){
  length(y) -> n
  c_term <- - n * log(2 * pi) / 2
  logdet_term <- - log(det(var)) / 2
  res_term <- - t(yvec - Xmat %*% betahat) %*% solve(var) %*% (yvec - Xmat %*% betahat) / 2
  return(c_term + logdet_term + res_term)
}

