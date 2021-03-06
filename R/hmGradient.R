# devtools::document()

#' creates a heatmap-ready data.frame
#' 
#' Imports:
#' grDevices
#'
#' @param highcol string, color of your choice for high values
#' @param midcol string, color of your choice for middle values
#' @param lowcol string, color of your choice for low values
#' @param n integer, half this number is used as an input for the colorRampPalette function for each gradient (high to mid and mid to low)
#' @return a character vector of length n, containing all colors from high to low
#' @export
#' @examples
#' library(ggdendroplot)
#'
#' hmGradient()
#'
hmGradient <- function(highcol="red", midcol="white", lowcol="blue", n=100){
  n2 <- ceiling(n/2)
  colfunc1 <- grDevices::colorRampPalette(c(lowcol, midcol)) #functions for getting a color ramp
  colfunc2 <- grDevices::colorRampPalette(c(midcol, highcol))
  colorgradient<- c(colfunc1(n2), colfunc2(n2))
  
  #==output==#
  return(colorgradient)
}