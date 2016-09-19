#' Calculate LODs - two profile and one joint - for two traits
#'
#' @param log10det_rss tibble containing log10determinants of RSS
#' @param n_mouse sample size
#' @export

calc_lod <- function(data, n_mouse){
  # define log10detrss0
  log10det_rss0 <- data %>%
    dplyr::select(marker1 == marker2)
  argmin <- data %>%
    filter(log10detrss) %>%
    which.min
  profile1 <- data %>%
    dplyr::select(marker1 == argmin$marker1)
  profile2 <- data %>%
    dplyr::select(marker2 == argmin$marker2)
  row_mins <- apply(FUN = min, X = log10det_rss, MARGIN = 1)
  # find min value per column of logrss
  log10det_rss_col_mins <- apply(FUN = min, X = log10det_rss, MARGIN = 2)
  p1 <- calc_profile_lod(log10det_rss_mins = log10det_rss_row_mins, log10det_rss0 = log10det_rss0, n_mouse = n_mouse)
  p2 <- calc_profile_lod(log10det_rss_mins = log10det_rss_col_mins, log10det_rss0 = log10det_rss0, n_mouse = n_mouse)
  joint <- calc_profile_lod(log10det_rss_mins = log10det_rss0, log10det_rss0 = log10det_rss0, n_mouse = n_mouse)
  return(tibble::tibble(p1, p2, joint))
}
