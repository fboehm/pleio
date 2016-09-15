#' Calculate profile LODs for two traits
#'
#' @param log10det_rss matrix containing log10determinants of RSS
#' @param n_mouse sample size
#' @export

calc_profile_lods <- function(log10det_rss, n_mouse){
  log10det_rss0 <- diag(log10det_rss)
  log10det_rss_row_mins <- apply(FUN = min, X = log10det_rss, MARGIN = 1)
  # find min value per column of logrss
  log10det_rss_col_mins <- apply(FUN = min, X = log10det_rss, MARGIN = 2)
  p1 <- calc_profile_lod(log10det_rss_mins = log10det_rss_row_mins, log10det_rss0 = log10det_rss0, n_mouse = n_mouse)
  p2 <- calc_profile_lod(log10det_rss_mins = log10det_rss_col_mins, log10det_rss0 = log10det_rss0, n_mouse = n_mouse)
  joint <- calc_profile_lod(log10det_rss_mins = log10det_rss0, log10det_rss0 = log10det_rss0, n_mouse = n_mouse)
  return(list(p1, p2, joint))
}
