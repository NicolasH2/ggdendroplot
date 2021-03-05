# devtools::document()

#' create a dendrogram as several geom_path layers ready to be added to a ggplot object
#'
#' Imports:
#' ggplot2
#'
#' @inheritParams ggplot2::geom_path
#' @import ggplot2
#'
#' @param data data.frame, columns will be clustered based on their rows.
#' @param xlim, vector with 2 numbers, on the x axis the dendrogram will beginn at the first number and end at the second.
#' @param ylim, vector with 2 numbers, on the y axis the dendrogram will beginn at the first number and end at the second.
#' @param pointing, string, either "side" or "updown" (default) to indicate where the dendrogram should point.
#' @param clustmethod string, method to be used by the hclust function (check ?hclust for available methods).
#' @param axis.labels, boolean, whether or not the axis should show the column names of data. This adds another layer to define the axis labels.
#' @return a list of several ggplot2 layer objects (geom_path for the dendrogram) that can directly be added to a ggplot2 object
#' @details the function uses geom_path for the dendrogram, so ... takes all arguements that geom_path would also take, such as color, size, etc.
#' @export
#' @examples
#' library(ggdendroplot)
#' library(ggplot2)
#'
#' df <- matrix(rnorm(128), ncol = 8)
#' colnames(df) <- paste0("a",seq(ncol(df)))
#'
#' ggplot() + geom_dendro(data=df)
#' ggplot() + geom_dendro(data=df, pointing="side")
#' ggplot() + geom_dendro(data=df, xlim=c(3,0))
#' ggplot() + geom_dendro(data=df, ylim=c(3,0))
#' ggplot() + geom_dendro(data=df, size=2, color="blue", linetype="dashed")
#' ggplot() + geom_dendro(data=df, size=4, lineend="round")
#'
geom_dendro <- function(data, xlim=NULL, ylim=NULL, pointing="updown", clustmethod="complete", axis.labels=TRUE, ...){

  calc <- .plotcalculation(data=data, xlim=xlim, ylim=ylim, pointing=pointing, clustmethod=clustmethod)

  dendro <- calc[["dendro"]] #for the dendrogram
  plotlabels <- calc[["plotlabels"]] #for labels as axis text

  #plot many geom_paths to create one dendrogram; each path is an item in a list
  output <- lapply(dendro, function(x){
    return(ggplot2::layer(
      data=x,
      mapping=ggplot2::aes(x=x, y=y),
      geom="path",
      stat="identity",
      position="identity",
      show.legend=FALSE,
      params=list(...)
    ))
  })

  if(axis.labels){
    ggplotlabel <- switch(pointing,
                          "updown"=scale_x_continuous(breaks=plotlabels$x, labels=plotlabels$label),
                          "side"=scale_y_continuous(breaks=plotlabels$y, labels=plotlabels$label))

    output <- c(output, ggplotlabel)
  }

  return(output)
}

.plotcalculation <- function(data, xlim, ylim, pointing, clustmethod){
  distmatrix <- dist(t(data))
  clust <- hclust(distmatrix, method=clustmethod)

  ranks <- clust$order
  samples <- clust$labels[ranks]
  dflabel <- data.frame(label=samples, x=seq(length(samples)), y=0)

  confusingRanks <- sapply(seq(length(ranks)), function(x) which(ranks==x))

  #========================
  #df is a data.frame with one row for each arch
  df <- cbind(as.data.frame(clust$merge), y=clust$height)
  dfy <- data.frame(y1=apply(df, 1, function(x) ifelse(x[1]<0, 0, df[x[1],"y"])),
                    y2=apply(df, 1, function(x) ifelse(x[2]<0, 0, df[x[2],"y"])) )

  #needs to be a loop because the rows are calculated successively and depend on those that were calculated before
  dfx <- data.frame()
  for(i in 1:nrow(df)){
    dfx[i,1] <- ifelse(df[i,1]<0, confusingRanks[abs(df[i,1])], mean(c(dfx[df[i,1],1], dfx[df[i,1],2])))
    dfx[i,2] <- ifelse(df[i,2]<0, confusingRanks[abs(df[i,2])], mean(c(dfx[df[i,2],1], dfx[df[i,2],2])))
  }
  colnames(dfx) <- c("x1","x2")
  df <- cbind(df, dfy, dfx)

  #========================
  #df2 is a list of data.frames (one for each arch), listing their x and y coordinates for a geom_path
  df2 <- lapply(seq(nrow(df)), function(xyz){
    rx <- unlist(df[xyz,])
    return( data.frame(x=c(rx[6], rx[6], rx[7], rx[7]), y=c(rx[4], rx[3], rx[3], rx[5]), z=xyz ))
  })

  #========================
  #df3 rbinds all data.frames from df2, with one column for x, y and z (z just stats which arch it is)
  df3 <- do.call(rbind, df2)
  if(pointing %in% "side"){
    colnames(df3) <- c("y","x","z")
    colnames(dflabel) <- c("label","y","x")
  }

  extremes <- c(range(df3$x), range(df3$y))
  delta <- c(abs(extremes[2]-extremes[1]), abs(extremes[4]-extremes[3]))
  if(is.null(xlim)) xlim <- extremes[1:2]
  if(is.null(ylim)) ylim <- extremes[3:4]


  #relevel the coordinates from 0 to 1 for both x and y
  df4 <- df3
  df4$x <- (df3$x-extremes[1])/delta[1]
  df4$y <- (df3$y-extremes[3])/delta[2]
  dflabel$x <- (dflabel$x-extremes[1])/delta[1]
  dflabel$y <- (dflabel$y-extremes[3])/delta[2]

  #relevel the coordinates to fit xlim and ylim (if not user-defined, the original values are taken)
  df5 <- df4
  df5$x <- (xlim[2]-xlim[1])*df4$x + xlim[1]
  df5$y <- (ylim[2]-ylim[1])*df4$y + ylim[1]
  dflabel$x <- (xlim[2]-xlim[1])*dflabel$x + xlim[1]
  dflabel$y <- (ylim[2]-ylim[1])*dflabel$y + ylim[1]

  labelsInOrder=dflabel$label[order(dflabel$x, dflabel$y)]

  df6 <- split(df5, df5$z)

  output <- list(dendro=df6, plotlabels=dflabel, labelsInOrder=labelsInOrder)
  return(output)
}
