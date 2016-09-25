#' Calculate LODs - two profile and one joint - for two traits
#'
#' @param data tibble containing log10determinants of RSS
#' @param n_mouse sample size
#' @param true_profile_lik indicator (defaults to TRUE) of whether to calculate true profile lod, ie, by maximizing over the nuisance parameter $\lambda_i$
#' @export

calc_lod <- function(data, n_mouse, true_profile_lik = TRUE){
  # define log10detrss0
  log10detrss0 <- data %>%
    dplyr::filter(marker1 == marker2)
  if (!true_profile_lik){
    argmin <- data %>%
      dplyr::filter(log10detrss == min(log10detrss))
    profile1 <- data %>%
      dplyr::filter(marker1 == argmin$marker1)
    profile2 <- data %>%
      dplyr::filter(marker2 == argmin$marker2)
  }
  if (true_profile_lik){
    profile1 <- data %>%
      dplyr::group_by(marker2) %>%
      summarise(profile = min(log10detrss))
    profile2 <- data %>%
      dplyr::group_by(marker1) %>%
      summarise(profile = min(log10detrss))
  }
    return(tibble::tibble(lod1 = - n_mouse * (profile1$profile - min(log10detrss0$log10detrss)) / 2,
                        lod2 = - n_mouse * (profile2$profile - min(log10detrss0$log10detrss))/ 2,
                        joint = - n_mouse * (log10detrss0$log10detrss - min(log10detrss0$log10detrss)) / 2))
}
