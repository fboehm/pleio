#' Calculate LODs - two profile and one joint - for two traits
#'
#' @param data tibble containing log10determinants of RSS
#' @param n_mouse sample size
#' @export

calc_lod <- function(data, n_mouse){
  # define log10detrss0
  log10detrss0 <- data %>%
    dplyr::filter(marker1 == marker2)
  argmin <- data %>%
    dplyr::filter(log10detrss == min(log10detrss))
  profile1 <- data %>%
    dplyr::filter(marker1 == argmin$marker1)
  profile2 <- data %>%
    dplyr::filter(marker2 == argmin$marker2)
  return(tibble::tibble(lod1 = - n_mouse * profile1$log10detrss / 2,
                        lod2 = - n_mouse * profile2$log10detrss / 2,
                        joint = - n_mouse * log10detrss0$log10detrss / 2))
}
