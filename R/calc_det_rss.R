#' Calculate determinants of cross-product of residuals matrices
#'
#' @param res an array of residuals
#' @export

calc_det_rss <- function(res){
  n_marker <- dim(res)[3]
  det_rss <- matrix(nrow = n_marker, ncol = n_marker)
  for (i in 1:n_marker){
    for (j in 1:n_marker){
      m1 <- res[, 1, i]
      m2 <- res[, 2, j]
      mymat <- cbind(m1, m2)
      det_rss[i, j] <- det(t(mymat) %*% mymat)
    }
  }
  return(det_rss)
}
