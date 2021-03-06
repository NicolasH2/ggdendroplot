# devtools::document()

#' create a dendrogram in ggplot2
#'
#' Imports:
#' ggplot2
#'
#' @inheritParams ggplot2::geom_path
#' @import ggplot2
#'
#' @param clust hclust object.
#' @param xlim vector with 2 numbers, on the x axis the dendrogram will beginn at the first number and end at the second.
#' @param ylim vector with 2 numbers, on the y axis the dendrogram will beginn at the first number and end at the second.
#' @param pointing string, either "side" or "updown" (default) to indicate where the dendrogram should point.
#' @param axis.labels boolean, whether or not the axis should show the column names of data. This adds another layer to define the axis labels.
#' @param dendrocut integer, to use different colors in the dendrogram. The integer defines from which cluster downwards the color coding should occur. Imagine a line beeing drawn at that point. All clusters that originate from a cluster at that line will have the same color. The higher the integer the less colors you will see. Colors are preset but can also be defined by the user in the groupCol arguement. 
#' @param groupCol character vector, each string should define a color. Defines the colors to be used if dendrocut is defined.
#' @param failsafe, boolean, if TRUE prevents the user from reversing the order of the dendrogram. That is solely because the hmReady function provides a data.frame the will have x and y values according to the original order of the dendrogram.
#' @return a list of several ggplot2 layer objects (geom_path for the dendrogram) that can directly be added to a ggplot2 object
#' @details the function uses geom_path for the dendrogram, so ... takes all arguements that geom_path would also take, such as color, size, etc.
#' @export
#' @examples
#' library(ggdendroplot)
#' library(ggplot2)
#'
#' #test data.frame
#' df <- matrix(rnorm(128), ncol = 8)
#' colnames(df) <- paste0("a",seq(ncol(df)))
#'
#' #calculate a distance matrix and perform hierarchical clustering
#' distmatrix <- dist(t(df))
#' clust <- hclust(distmatrix)
#'
#' #produce the graph
#' ggplot() + geom_dendro(clust)
#'
#' #you can access the default label order from clust.
#' clust$labels[clust$order]
#'
#' Note that in geom_dendro, this order is reversed if the limits are defined accordingly (if the first number is greater than the second), e.g.:
#' ggplot() + geom_dendro(clust, xlim=c(3,0))
#'
#' #=================================
#' #other plot examples
#' ggplot() + geom_dendro(clust, pointing="side")
#' ggplot() + geom_dendro(clust, ylim=c(3,0))
#' ggplot() + geom_dendro(clust, size=2, color="blue", linetype="dashed")
#' ggplot() + geom_dendro(clust, size=4, lineend="round")
#' ggplot() + geom_dendro(clust, axis.labels = F)
#'
geom_dendro <- function(clust, xlim=NULL, ylim=NULL, pointing="updown", dendrocut=NULL, groupCols=NULL, axis.labels=TRUE, failsafe=TRUE, ...){
  
  if(failsafe){
    warningMessage <- "values would reverse the dendrogram order. The coordinates of the hmReady output will not match. If you want to proceede anyway turn the failsafe arguement to FALSE."
    if(!is.null(xlim)){ if(xlim[2]-xlim[1]<0 & pointing %in% "updown") stop(paste("The chosen xlim", warningMessage)) }
    if(!is.null(ylim)){ if(ylim[2]-ylim[1]<0 & pointing %in% "side") stop(paste("The chosen ylim", warningMessage)) }
  }
  #perform the clustering and calculation to get a list of data.frames that can individually be plotted via geom_path
  calc <- .plotcalculation(clust=clust, xlim=xlim, ylim=ylim, pointing=pointing)
  dendro <- calc[["dendro"]] #list of geom_path objects for the dendrogram
  plotlabels <- calc[["plotlabels"]] #for labels as axis text, either a scale_x_continuous or scale_y_continuous object
  archs <- calc[["archs"]]
  
  #plot many geom_paths to create one dendrogram; each path is an item in a list
  output <- lapply(dendro, function(subdendro){
    return(ggplot2::layer(
      data = subdendro,
      mapping = ggplot2::aes(x=x, y=y),
      geom = "path",
      stat = "identity",
      position = "identity",
      show.legend = FALSE,
      inherit.aes = FALSE,
      params=list(...)
    ))
  })
  
  if(!is.null(dendrocut)){
    archColors <- .colorcalculation(dendrocut=dendrocut, archs=archs, groupCols=groupCols)
    
    output <- mapply(function(subdendro, subcolor){
      subdendro$aes_params[["colour"]] <- subcolor
      return(subdendro)
    }, subdendro=output, subcolor=archColors, SIMPLIFY=FALSE)
  }
  
  #if desired by the user, define axis labels for x or y axis, depending on if the dendrogram points down/up or sideways
  if(axis.labels){
    ggplotlabel <- switch(pointing,
                          "updown"=ggplot2::scale_x_continuous(breaks=plotlabels$x, labels=plotlabels$label),
                          "side" = ggplot2::scale_y_continuous(breaks=plotlabels$y, labels=plotlabels$label))
    
    output <- c(output, ggplotlabel) #combine the dendrogram with the axis label information
  }
  
  return(output)
}

.colorcalculation <- function(dendrocut, archs, groupCols){
  archgroups <- list(group1=dendrocut)
  
  for(i in dendrocut:1){
    presence <- sapply(archgroups, function(x) any(x %in% i))
    if(any(presence)){
      groupname <- names(archgroups[presence])
    }else{
      groupname <- paste0("group",length(archgroups)+1)
      archgroups[[groupname]] <- i
    }
    
    archgroups[[groupname]] <- as.vector(unlist(c( archgroups[[groupname]], archs[i,1:2] )))
  }
  
  finalGroups <- c()
  for(i in 1:nrow(archs)){
    findArch <- sapply(archgroups, function(x) any(x %in% i)) #findArch is a vector that contains a boolean for each group with the name of the group
    findArch <- names(findArch[findArch]) #gets the name of the group that has a TRUE (i.e. that contains the arch number)
    findArch <- ifelse(identical(findArch, character(0)), "group0", findArch)
    finalGroups <- c(finalGroups, findArch)
  }
  uniqueGroups <- unique(finalGroups)
  if(is.null(groupCols)){
    groupCols <- rep(c("blue","red","purple","darkgreen","orange","cyan4","lightsalmon4","darkseagreen"), ceiling(length(uniqueGroups)/8) )
    groupCols <- c(groupCols[1:length(uniqueGroups)-1], "black")
  }else{
    groupCols <- groupCols[1:length(uniqueGroups)]
  }
  names(groupCols) <- uniqueGroups
  archColors <- groupCols[finalGroups]
}

.plotcalculation <- function(clust, xlim, ylim, pointing){
  #========================
  #extract information from the hierarchical clustering
  ranks <- clust$order #column numbers in the order in which they occur in the dendrogram
  samples <- clust$labels[ranks] #column names in the order that is given by ranks
  if(is.null(samples)) stop("Please provide column names before you calculate the distance matrix.")
  dflabel <- data.frame(label=samples, x=seq(length(samples)), y=0) #data.frame with the columns names, and their positions in x (1-n) and y (all 0). This data.frame can later easily be modified in the same way as the dendrogram dataframe (shifting and rotating in the x-y space)
  
  #confusingRanks gives the positions of each column in the original column order
  confusingRanks <- sapply(seq(length(ranks)), function(x) which(ranks==x))
  
  #========================
  #df is a data.frame with one row for each arch
  df <- cbind(as.data.frame(clust$merge), y=clust$height )
  dfy <- data.frame(y1=apply(df, 1, function(x) ifelse(x[1]<0, 0, df[x[1],"y"])),
                    y2=apply(df, 1, function(x) ifelse(x[2]<0, 0, df[x[2],"y"])) )
  
  #needs to be a loop because the rows are calculated successively and depend on those that were calculated before
  dfx <- data.frame()
  for(i in 1:nrow(df)){
    dfx[i,1] <- ifelse(df[i,1]<0, confusingRanks[abs(df[i,1])], mean(c(dfx[df[i,1],1], dfx[df[i,1],2]))) #dfx[df[i,1],1] extractes the x1 position of the arch to which this arch is pointing (which has been assigned in an earlier iteration of the loop)
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
  #df3 rbinds all data.frames from df2, with one column for x, y and z (z just states which arch it is)
  df3 <- do.call(rbind, df2)
  if(pointing %in% "side"){
    colnames(df3) <- c("y","x","z")
    colnames(dflabel) <- c("label","y","x")
  }
  
  extremes <- c(range(df3$x), range(df3$y))
  delta <- c(abs(extremes[2]-extremes[1]), abs(extremes[4]-extremes[3]))
  if(is.null(xlim)) xlim <- extremes[1:2]
  if(is.null(ylim)) ylim <- extremes[3:4]
  
  #========================
  #df4 relevels the coordinates from 0 to 1 for both x and y
  df4 <- df3
  df4$x <- (df3$x-extremes[1])/delta[1]
  df4$y <- (df3$y-extremes[3])/delta[2]
  dflabel$x <- (dflabel$x-extremes[1])/delta[1]
  dflabel$y <- (dflabel$y-extremes[3])/delta[2]
  
  #========================
  #df5 relevels the coordinates to fit xlim and ylim (if not user-defined, the original values are taken)
  df5 <- df4
  df5$x <- (xlim[2]-xlim[1])*df4$x + xlim[1]
  df5$y <- (ylim[2]-ylim[1])*df4$y + ylim[1]
  dflabel$x <- (xlim[2]-xlim[1])*dflabel$x + xlim[1]
  dflabel$y <- (ylim[2]-ylim[1])*dflabel$y + ylim[1]
  
  labelsInOrder=dflabel$label[order(dflabel$x, dflabel$y)]
  
  #========================
  #df6 splits the data.frame into list, one item for each arch
  df6 <- split(df5, df5$z)
  
  output <- list(dendro=df6, plotlabels=dflabel, archs=df)
  return(output)
}

