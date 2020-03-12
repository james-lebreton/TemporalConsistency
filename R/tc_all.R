#' Estimates of Temporal Consistency from DeSimone (2015)
#'
#' This function estimates test-level and item-level temporal consistency
#' using: test-retest correlations, component loadings, square root mean
#' residual, and D^2 for personal temporal consistency.
#'
#' Link to DeSimone(2015): https://journals.sagepub.com/doi/full/10.1177/1094428114553061
#'
#' @param id Case ID variable
#' @param tot1 Total test score at time 1
#' @param tot2 Total test score at time 1
#' @param it1 Item scores at time 1
#' @param it2 Item scores at time 2
#' @param printD2 Logical value indicating whether or not to print the results of the
#'   d_ptc function (D, D2, & p-value of D2)
#' @return List of results
#' @export
#' @examples
#' data(desimone1)
#' names(desimone1)
#' id <- desimone1[,1]
#' t1items <- desimone1[,c(2:6)]
#' t1total <- desimone1[,7]
#' t2items <- desimone1[,c(8:12)]
#' t2total <- desimone1[,13]
#' tc_all(id, t1items, t1total, t2items, t2total, printD2=FALSE)
#' tc_all(id, t1items, t1total, t2items, t2total, printD2=TRUE)
tc_all <- function(id, it1, tot1, it2, tot2, printD2 = FALSE){
  output1 <- cor_tc(it1, tot1,it2,tot2)
  output2 <- srmr_tc(it1,it2)
  output3 <- cl_tc(it1,it2)
  if (printD2==TRUE){
    output4 <- d_ptc(id,it1,it2)
    TemporalConsistency <- list(c(Test.Retest.Correlations = output1,
                                  Square.Root.Mean.Residual = output2,
                                  Component.Loading.Similarity = output3,
                                  Person.Temporal.Consistency = output4))
    return(TemporalConsistency)
  }
  TemporalConsistency <- list(c(Test.Retest.Correlations = output1,
                                Square.Root.Mean.Residual = output2,
                                Component.Loading.Similarity = output3))
  return(TemporalConsistency)
}
