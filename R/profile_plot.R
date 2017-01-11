#' Plot profile LR statistics for two traits
#'
#' @param profiles n x 3 tibble containing profile lod statistics for two traits and the joint LOD
#' @param marker_positions vector of marker positions (should have length equal to that of lod1 & lod2)
#' @param qtl_positions vector of length 2 (if not NULL) specifying the positions of the two loci
#' @param trait1name character vector of length 1 that specifies the name of trait 1
#' @param trait2name character vector of length 1 that specifies the name of trait 2
#' @export

profile_plot <- function(profiles, marker_positions, trait1name = "Simulated trait 1", 
                         trait2name = "Simulated trait 2", qtl_positions = NULL){
  df1 <- dplyr::bind_cols(profiles, tibble::as_tibble(marker_positions))
  df <-  dplyr::rename(df1, marker_positions = value)
  pp <- ggplot2::ggplot(df)  +
    ggplot2::geom_line(ggplot2::aes(colour = trait1name, linetype = trait1name,
                                    x = marker_positions, y = lod1), linetype = 2) +
    ggplot2::geom_line(ggplot2::aes(colour = trait2name, linetype = trait2name, 
                                    x = marker_positions, y = lod2), linetype = 3) +
    ggplot2::geom_line(ggplot2::aes(colour = "Pleiotropy", linetype = "Pleiotropy", 
                                    x = marker_positions, y = joint), linetype = 1) +
    ggplot2::labs(colour = "Legend", linetype = "Legend", x = "Marker position (Mb)", y = "LOD") +
    ggplot2::geom_vline(xintercept = qtl_positions[1]) +
    ggplot2::geom_vline(xintercept = qtl_positions[2])
  return(pp)
}
