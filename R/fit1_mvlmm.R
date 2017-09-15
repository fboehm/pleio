#' Fit a 2-locus, multivariate trait linear mixed effects model
#' 
#' @param Y a phenotypes matrix with one row per subject and one trait per column
#' @param X1 genotype probabilities for first locus, one row per subject
#' @param X2 genotype probabilities for second locus, one row per subject
#' @param Kmat a kinship matrix
#' @param tol value of tol to be passed to regress()
#' @param maxcyc value of maxcyc passed to regress()
#' @param verbose value of verbose passed to regress()
#' @param start value of start passed to regress()
#' @param taper value of taper passed to regress()
#' @export

fit1_bvlmm <- function(Y, X1, X2, Kmat, tol = 0.0000001, maxcyc = 100, verbose = 10, start = c(0.1, 0.1, 0, 1, 1, 0), taper = rep(1 / 2, maxcyc)){
  # assemble Xmat design matrix
  n <- nrow(Y)
  Xmat <- pleiotropy::stagger_mats(X1, X2)
  # fit mvlmm
  In <- diag(1, n)
  V1 <- matrix(c(1, 0, 0, 0), nrow = 2)
  V2 <- matrix(c(0, 0, 0, 1), nrow = 2)
  V3 <- matrix(c(0, 1, 1, 0), nrow = 2)
  # define matrices 
  K1 <- V1 %x% Kmat
  K2 <- V2 %x% Kmat
  K3 <- V3 %x% Kmat
  I1 <- V1 %x% In
  I2 <- V2 %x% In
  I3 <- V3 %x% In
  # call regress
  out <- regress::regress(as.matrix(as.vector(Y)) ~ Xmat - 1,
   #              ~ K1 + K2 + K3 + I1 + I2 + I3,
                 ~ K1 + K2 + K3 + I1 + I2 + I3,
                 identity = FALSE,
                 pos = c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE),
                 #pos = FALSE, 
                 verbose = verbose,
                 start = start, 
                 tol = tol, 
                 maxcyc = maxcyc, 
                 taper = taper
                 )
  return(out)
}