# devtools::document()

#' creates a heatmap-ready data.frame
#'
#' Imports:
#' grDevices
#'
#' @param highcol string, color of your choice for high values
#' @param midcol string, color of your choice for middle values
#' @param lowcol string, color of your choice for low values
#' @param bend integer, bends the color scale so that the middle color takes up less space the higher the bend value is
#' @param n integer, half this number is used as an input for the colorRampPalette function for each gradient (high to mid and mid to low)
#' @return a character vector of length n, containing all colors from high to low
#' @export
#' @examples
#' library(ggdendroplot)
#'
#' hmGradient()
#'
hmGradient <- function(highcol="red", midcol="white", lowcol="blue", bend=1, n=100){
  n <- ceiling(n/2)
  colfunc1 <- grDevices::colorRampPalette(c(lowcol, midcol)) #functions for getting a color ramp
  colfunc2 <- grDevices::colorRampPalette(c(midcol, highcol))
  keep1 <- round(seq(1,(n)^(1/bend),length.out=n)^bend)
  keep2 <- rev(n-keep1)
  col1 <- colfunc1(n)[keep1]
  col2 <- colfunc2(n)[keep2]
  
  colorgradient <- c(col1,col2)
  
  #==output==#
  return(colorgradient)
}
