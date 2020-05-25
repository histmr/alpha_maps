#' An Alpha shape mapping function
#'
#' This function takes as series of lon lat points and returns a data frame with the coordinates and group of an alpha shape
#' @param lon_lat A two column dataframe or matrix with spatial coordinates
#' @param alpha The alpha parameter for the package alphahull
#' @keywords alpha shape
#' @keywords spatial
#' @keywords map
#' @import alphahull
#' @import igraph
#' @import data.table



alpha_map <- function(lon_lat, alpha=0.5){
  alpha_obj <- ashape(lon_lat, alpha = alpha)

  network <-  graph.edgelist(cbind(as.character(alpha_obj$edges[, "ind1"]), as.character(alpha_obj$edges[, "ind2"])), directed = FALSE)
  group.df <- data.frame("group"=components(network, "strong")$membership)
  group.df$IDs <- names(components(network, "strong")$membership)


  alpha.df <- data.frame(alpha_obj$edges)
  alpha_temp.df <- data.frame("IDs"= c(alpha.df$ind1,alpha.df$ind2), "x"=c(alpha.df$x1,alpha.df$x2),"y"=c(alpha.df$y1,alpha.df$y2))


  ## all three parts as a list
  segments.ls<- lapply(1:clusters(network)$no, function (x) atan_sort(alpha_temp.df[which(alpha_temp.df$IDs%in%group.df$IDs[which(group.df$group==x)]),]))
  # segments.ls<- lapply(1:1, function (x) atan_sort(alpha_temp.df[which(alpha_temp.df$IDs%in%group.df$IDs[which(group.df$group==x)]),]))

  segments.df <- rbindlist(segments.ls)
  ## now assign groups and unlist to data frame

  group_vector <- NULL
  for (i in 1:clusters(network)$no)
  {
    temp <- rep(i,nrow(segments.ls[[i]]))
    group_vector <- c(group_vector,temp)
  }

  segments.df$group <- group_vector
  return(segments.df)
}
