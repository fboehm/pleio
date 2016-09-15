#' Calculate a profile lod vector
#'
#' @param log10det_rss_mins a vector of log10 determinant minima by row or by column
#' @param log10det_rss0 a vector of log10 determinant under pleiotropy at distinct loci
#' @param n_mouse number of mice for likelihood calculations
#' @export

calc_profile_lod <- function(log10det_rss_mins, log10det_rss0, n_mouse){
  - n_mouse * (log10det_rss_mins - min(log10det_rss0)) / 2
}
