#' applies a heatmap-friendly theme to ggplot
#'
#' Imports:
#' ggplot2
#'
#' @import ggplot2
#'
#' @return a ggplot layer that can be added to a ggplot object
#' @export
#' @examples
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   theme_hm()
#'
theme_hm <- function(){
  return(
    ggplot2::theme(
      panel.grid =       element_blank(),
      panel.background = element_blank(),
      axis.ticks =       element_blank(),
      axis.text.y =      element_text( color="black" ),
      axis.text.x =      element_text( angle=45, hjust=1, color="black" )
    )
  )
}
