#' Assemble into a matrix Vg from fit1_bvlmm() output
#' 
#' @param sigma Vector of output from fit1_bvlmm(); estimates of variance components
#' @export

assemble_Vg <- function(sigma){
  matrix(data = c(sigma[1], sigma[3], sigma[3], sigma[2]), nrow = 2, ncol = 2)
}

#' Assemble into a matrix Ve from fit1_bvlmm() output
#' 
#' @param sigma Vector of output from fit1_bvlmm(); estimates of variance components
#' @export

assemble_Ve <- function(sigma){
  matrix(data = c(sigma[4], sigma[6], sigma[6], sigma[5]), nrow = 2, ncol = 2)
}