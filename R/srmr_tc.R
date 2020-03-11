#' Square Root Mean Residual for Temporal Consistency
#'
#' This function estimates item-level temporal consistency using
#' the square root mean residual (SRMR) for as recommended by DeSimone(2015).
#' The input consistes of two data frames containing the focal items
#' of interest at time 1 (df1) and time 2 (df2).
#'
#' @param it1 Columns in a dataframe or matrix containing the items measured at time 1
#' @param it2 Columns in a dataframe or matrix containing the items measured at time 2
#' @return Estimate of SRMR for item-level test-retest correlations
#' @export

srmr_tc <- function(it1,it2) {
  t1.cor <- stats::cor(it1) # estimate correlations between items at time 1
  t1.cor[upper.tri(t1.cor)] <-NA # setting redundant correlations to NA; keeping only unique elements
  t2.cor <- stats::cor(it2) # estimate correlations between items at time 2
  t2.cor[upper.tri(t2.cor)] <-NA # setting redundant correlations to NA
  dif <- (t1.cor-t2.cor) # compute differences across time
  sq.dif <- dif^2 # squared differences
  ss.dif <- sum(sq.dif,na.rm=T) # sum of squared differences
  k <- (ncol(t1.cor)*(ncol(t1.cor)+1)/2) # k is the number of unique elements in correlation matrix
  mr <- sum(ss.dif)/k
  srmr.tc <- sqrt(mr)
  return(srmr.tc)
}
