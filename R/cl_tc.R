#' Component Loadings for Temporal Consistency
#'
#' This function estimates item-level temporal consistency
#' using component loadings as recommended by DeSimone(2015).
#' The input consistes of two data frames containing the focal items
#' of interest at time 1 (it1) and time 2 (it2).
#' @param it1 Columns in a dataframe or matrix containing the items measured at time 1
#' @param it2 Columns in a dataframe or matrix containing the items measured at time 2
#' @return Temporal consistency estimated as the similarity across component loadings
#' @export
cl_tc <- function(it1,it2) {
  t1.pca <- psych::principal(it1,nfactors=ncol(it1),rotate="none") # PCA; extract #components = # of items
  t2.pca <- psych::principal(it2,nfactors=ncol(it2),rotate="none") # PCA; extract #components = # of items
  w <- (t1.pca$values+t2.pca$values)/(2*ncol(it1)) # compute the weights
  r <- stats::cor(t1.pca$loadings,t2.pca$loadings) # obtain correlations between item-PCA loadings at time 1 and time 2
  r2 <- diag(r)^2
  r2*w
  cl.tc <- sum(r2*w)
  return(cl.tc)
}
