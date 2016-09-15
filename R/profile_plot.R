#' Plot profile LR statistics for two traits
#'
#' @param profile1 profile lod statistic for trait one
#' @param profile2 profile lod statistic for trait two
#' @param marker_positions vector of marker positions (should have length equal to that of profile1 & profile2)
#' @param col1 color of trait 1 plot
#' @param col2 color of trait 2 plot
#' @export


profile_plot <- function(profile1, profile2, marker_positions, col1 = "red", col2 = "blue"){
  df <- data.frame(profile1, profile2, marker_positions)
  ggplot2::ggplot(df)  +
    ggplot2::geom_line(colour = col1, ggplot2::aes(x = marker_positions, y = profile1)) +
    ggplot2::geom_line(colour = col2, ggplot2::aes(x = marker_positions, y = profile2)) +
    ggplot2::labs(x = "Marker position (Mb)", y = "Profile LOD")
}
