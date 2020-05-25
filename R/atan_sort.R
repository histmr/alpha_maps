#' A helper function for ordering segments by atan
#' This function returns ordered segments for a polygon
#'
#' @param df A two column dataframe or matrix with spatial coordinates



atan_sort <- function(df)
{
  df <- df[order(-1 * atan2(
    df$y - mean(range(df$y)),
    df$x - mean(range(df$x)))), c("x", "y")]
  df <- rbind.data.frame(df[nrow(df),], df[1:(nrow(df)-1),])
  df
}

