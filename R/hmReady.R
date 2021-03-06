# devtools::document()

#' creates a heatmap-ready data.frame
#'
#' Imports:
#' reshape2
#'
#' @param df data.frame
#' @param rowclus hclust object, use only if rows should be clustered
#' @param colclus hclust object, use only if columns should be clustered
#' @return a data.frame with x, y and value column, ready to be plotted by geom_tile.
#' @export
#' @examples
#' library(ggdendroplot)
#'
#' df <- as.data.frame(matrix(rnorm(99), ncol = 9))
#' rownames(df) <- paste0("trait", seq(nrow(df)))
#' 
#' clust <- hclust(dist(df))
#' hm <- hmReady(df, rowclus=clust)
#' 
#' ggplot(hm, aes(x, y, fill=value)) + geom_tile()
#'
hmReady <- function(df, rowclus=NULL, colclus=NULL){
  df <- as.data.frame(df)
  
  if(is.null(rowclus)){
    rowsort <- rownames(df)
  }else{
    rowsort <- rowclus$labels[rowclus$order]
  }
  
  if(is.null(colclus)){
    colsort <- colnames(df)
  }else{
    colsort <- colclus$labels[colclus$order]
  }
  
  rowToNumber <- seq(nrow(df))
  names(rowToNumber) <- rowsort
  colToNumber <- seq(ncol(df))
  names(colToNumber) <- colsort
  
  df2 <- cbind(rowid=rownames(df), df)
  df2 <- reshape2::melt(df2, id.vars="rowid")
  df2$y <- rowToNumber[as.character(df2$rowid)]
  df2$x <- colToNumber[as.character(df2$variable)]
  
  return(df2)
}
