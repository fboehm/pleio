#' Calculate LODs - two profile and one joint - for two traits
#'
#' @param data tibble containing log10determinants of RSS
#' @param n_mouse sample size
#' @export
#' @importFrom rlang .data

calc_lod <- function(data, n_mouse){
  # define log10detrss0
  log10detrss0 <- dplyr::filter(data, .data$marker1 == .data$marker2)
  pre1 <- dplyr::group_by(data, .data$marker1)
  profile1 <- dplyr::summarise(pre1, profile = min(.data$log10detrss))
  pre2 <- dplyr::group_by(data, .data$marker2)
  profile2 <- dplyr::summarise(pre2, profile = min(.data$log10detrss))
  return(tibble::tibble(lod1 = - n_mouse * (profile1$profile - min(log10detrss0$log10detrss)) / 2,
                        lod2 = - n_mouse * (profile2$profile - min(log10detrss0$log10detrss))/ 2,
                        joint = - n_mouse * (log10detrss0$log10detrss - min(log10detrss0$log10detrss)) / 2))
}
