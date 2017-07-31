#' Prepare phenotype for use with scan functions
#' 
#' @param phenotype a phenotype vector with values for a single trait
#' @param alpha input to winsorize
#' @param transform function to apply to phenotype
#' @export
#' 

prep_pheno <- function(phenotype, alpha = 0.02, transform = log){
    foo <- broman::winsorize(phenotype, q = alpha)
    out <- transform(foo)
    return(out)
}