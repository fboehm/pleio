#' Plot profile LR statistics for two traits
#'
#' @param profiles list containing profile lod statistics for two traits and the joint LOD
#' @param marker_positions vector of marker positions (should have length equal to that of profile1 & profile2)
#' @param simulated_positions vector of length 2 (if not NULL) specifying the positions of the two loci in a simulation study
#' @export


profile_plot <- function(profiles, marker_positions, trait1name = "Simulated trait 1", 
                         trait2name = "Simulated trait 2", qtl_positions = NULL){
  profile1 <- profiles[, 1]
  profile2 <- profiles[, 2]
  joint <- profiles[, 3]
  df <- tibble::tibble(profile1, profile2, joint, marker_positions)
  if (is.null(qtl_positions)){
    pp <- ggplot2::ggplot(df)  +
      ggplot2::geom_line(ggplot2::aes(colour = trait1name, x = marker_positions, y = profile1), linetype = 2) +
      ggplot2::geom_line(ggplot2::aes(colour = trait2name, x = marker_positions, y = profile2), linetype = 3) +
      ggplot2::geom_line(ggplot2::aes(colour = "Pleiotropy", x = marker_positions, y = joint), linetype = 1) +
      ggplot2::labs(colour = "Legend", x = "Marker position (Mb)", y = "LOD")
  }
  if (!is.null(qtl_positions)){
    pp <- ggplot2::ggplot(df)  +
      ggplot2::geom_line(ggplot2::aes(colour = trait1name, x = marker_positions, y = profile1), linetype = 2) +
      ggplot2::geom_line(ggplot2::aes(colour = trait2name, x = marker_positions, y = profile2), linetype = 3) +
      ggplot2::geom_line(ggplot2::aes(colour = "Pleiotropy", x = marker_positions, y = joint), linetype = 1) +
      ggplot2::labs(colour = "Legend", x = "Marker position (Mb)", y = "LOD") +
      ggplot2::geom_vline(xintercept = qtl_positions[1]) +
      ggplot2::geom_vline(xintercept = qtl_positions[2])
  }
  return(pp)
}
