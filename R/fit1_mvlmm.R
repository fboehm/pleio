#' Fit a 2-locus, multivariate trait linear mixed effects model
#' 
#' @param Y a phenotypes matrix with one row per subject and one trait per column
#' @param X1 genotype probabilities for first locus, one row per subject
#' @param X2 genotype probabilities for second locus, one row per subject
#' @param Kmat a kinship matrix
#' @param kernel value of kernel to be passed to regress()
#' @export

fit1_bvlmm <- function(Y, X1, X2, Kmat, kernel = NULL){
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
  V12 <- matrix(c(1, 0, 0, 1), nrow = 2)
  V3 <- matrix(c(0, 1, 1, 0), nrow = 2)
  if (reml == TRUE) {
    kernel <- NULL
  }
  else {
    kernel <- 0
  }
  K12 <- V12 %x% Kmat
  K3 <- V3 %x% Kmat
  I1 <- V1 %x% In
  I2 <- V2 %x% In
  I3 <- V3 %x% In
  
  out <- regress::regress(as.matrix(as.vector(Y)) ~ Xmat - 1,
   #              ~ K1 + K2 + K3 + I1 + I2 + I3,
                 ~ K12 + K3 + I1 + I2 + I3,
                #~ diag(1, 2) %x% Kmat + V1 %x% In + V2 %x% In + V3 %x% In,
                 identity = FALSE,
                 
                 pos = c(TRUE, FALSE, TRUE, TRUE, FALSE),
                 #pos = c(TRUE, TRUE, TRUE, FALSE)
                 verbose = 10,
                 start = c(1, 0, 1, 1, 0)
                 )
  return(out)
}