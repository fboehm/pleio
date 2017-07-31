#' Fit a 2-locus, multivariate trait linear mixed effects model
#' 
#' @param Y a phenotypes matrix with one row per subject and one trait per column
#' @param X1 genotype probabilities for first locus, one row per subject
#' @param X2 genotype probabilities for second locus, one row per subject
#' @param Kmat a kinship matrix
#' @param reml indicator of whether to use REML method
#' @export

fit1_bvlmm <- function(Y, X1, X2, Kmat, reml = FALSE){
  # assemble Xmat design matrix
  n <- nrow(Y)
  n_founders <- ncol(X1)
  Xmat <- matrix(0, nrow = 2 * n, ncol = 2 * n_founders)
  Xmat[1:n, 1:n_founders] <- X1
  Xmat[(n + 1):(2 * n), (n_founders + 1):(2 * n_founders)] <- X2
  # fit mvlmm
  In <- diag(1, n)
  V1 <- matrix(c(1, 0, 0, 0), nrow = 2)
  V2 <- matrix(c(0, 0, 0, 1), nrow = 2)
  V3 <- matrix(c(0, 1, 1, 0), nrow = 2)
  if (reml == TRUE) {
    out <- regress::regress(as.matrix(as.vector(Y)) ~ Xmat - 1,
                 ~ diag(1, 2) %x% Kmat + V1 %x% In + V2 %x% In + V3 %x% In,
                 identity = FALSE,
                 kernel = NULL, 
                 pos = c(TRUE, TRUE, TRUE, FALSE)
                 )
  }
  else {
    out <- regress::regress(as.matrix(as.vector(Y)) ~ Xmat - 1,
                            ~ diag(1, 2) %x% Kmat + V1 %x% In + V2 %x% In + V3 %x% In,
                            identity = FALSE,
                            kernel = 0, 
                            pos = c(TRUE, TRUE, TRUE, FALSE)
                            )
    }
  return(out)
}