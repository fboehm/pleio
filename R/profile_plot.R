#' Plot profile LR statistics for two traits
#'
#' @param profile1 profile lr statistic for trait one
#' @param profile2 profile lr statistic for trait two
#' @param marker_positions vector of marker positions (should have length equal to that of profile1 & profile2)
#' @export


profile_plot <- function(profile1, profile2, marker_positions, col1 = "red", col2 = "blue"){
  df <- data.frame(profile1, profile2, marker_positions)
  ggplot(df)  +
    geom_line(colour = col1, aes(x = marker_positions, y = profile1)) +
    geom_line(colour = col2, aes(x = marker_positions, y = profile2)) +
    labs(x = "Marker position", y = "Profile lr_stat")
}
