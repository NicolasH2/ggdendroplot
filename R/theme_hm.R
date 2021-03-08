#' applies a heatmap-friendly theme to ggplot
#'
#' Imports:
#' ggplot2
#'
#' @inheritParams ggplot2::geom_path
#' @import ggplot2
#'
#' @return a ggplot layer that can be added to a ggplot object
#' @export
#' @examples
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   theme_hm()
#'
theme_hm <- function(hmReady){
  return(
    ggplot2::theme(
      panel.grid =       ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.ticks =       ggplot2::element_blank(),
      axis.text.y =      ggplot2::element_text( color="black" ),
      axis.text.x =      ggplot2::element_text( angle=45, hjust=1, color="black" )
    )
  )
}
