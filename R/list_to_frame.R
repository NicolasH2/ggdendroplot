# devtools::document()

#' create a data.frame from a named list
#'
#' @param setlist a named list of character vectors.
#' @return a data.frame. Each column represents an item from the input list. Each row is a string that was present in the input lists. For each row, the items get a value of how many times the contained this rowname.
#' @export
#' @examples
#' library(ggdendroplot)
#'
#' set1 <- rownames(subset(mtcars, mpg>18))
#' set2 <- rownames(subset(mtcars, qsec>18))
#' set3 <- rownames(subset(mtcars, cyl<5))
#' carset <- list(highMpg=set1, highQsec=set2, lowCyl=set3)
#'
#' df <- list_to_frame(carset)
#'
list_to_frame <- function(setlist){

  l1 <- mapply(function(x,y){
    out <- as.data.frame(table(x))
    colnames(out) <- c("item",y)
    return(out)
  }, x=setlist, y=names(setlist), SIMPLIFY=FALSE)

  l2 <- Reduce(function(x,y) merge(x,y,by="item",all=T), l1)
  l2 <- as.matrix(l2)
  l2[is.na(l2)] <- 0
  l2 <- as.data.frame(l2)
  rownames(l2) <- l2$item
  l2$item <- NULL

  return(l2)
}
