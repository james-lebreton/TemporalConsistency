#' Estimates of Temporal Consistency from DeSimone (2015)
#'
#' This function estimates test-level and item-level temporal consistency
#' using: test-retest correlations, component loadings, square root mean
#' residual, and D^2 for personal temporal consistency.
#' @param tot1 Total test score at time 1
#' @param tot2 Total test score at time 1
#' @param it1 Item scores at time 1
#' @param it2 Item scores at time 2
#' @return List of results
#' @export
tc_all <- function(tot1, tot2, it1,it2) {
  output1 <- cor_tc(tot1, tot2,it1,it2)
  output2 <- srmr_tc(it1,it2)
  output3 <- cl_tc(it1,it2)
  TemporalConsistency <- list(c(Test.Retest.Correlations = output1, Square.Root.Mean.Residual = output2, Component.Loading.Similarity = output3))
  return(TemporalConsistency)
}
