#' Plot profile LR statistics for two traits
#'
#' @param profiles list containing profile lod statistics for two traits and the joint LOD
#' @param marker_positions vector of marker positions (should have length equal to that of profile1 & profile2)
#' @export


profile_plot <- function(profiles, marker_positions){
  profile1 <- profiles[, 1]
  profile2 <- profiles[, 2]
  joint <- profiles[, 3]
  df <- data.frame(profile1, profile2, joint, marker_positions)
  ggplot2::ggplot(df)  +
    ggplot2::geom_line(ggplot2::aes(colour = "Profile1", x = marker_positions, y = profile1)) +
    ggplot2::geom_line(ggplot2::aes(colour = "Profile2", x = marker_positions, y = profile2)) +
    ggplot2::geom_line(ggplot2::aes(colour = "Pleiotropy", x = marker_positions, y = joint)) +
    ggplot2::labs(colour = "Legend", x = "Marker position (Mb)", y = "LOD")

}
