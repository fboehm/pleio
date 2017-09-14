#' Stagger two matrices of same size within a larger, block-diagonal matrix
#'
#' @param X1 first matrix
#' @param X2 second matrix, of same size as first
#' @export

stagger_mats <- function(X1, X2){
  n_mouse <- nrow(X1)
  n_founders <- ncol(X1)
  out <- matrix(data = 0, nrow = 2 * n_mouse, ncol = 2 * n_founders)
  out[1:n_mouse, 1:n_founders] <- X1
  out[(n_mouse + 1):(2 * n_mouse), (n_founders + 1):(2 * n_founders)] <- X2
  return(out)
}
