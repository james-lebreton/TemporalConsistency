#' Test-Level and Item-Level Temporal Consistency Correlations
#'
#' This function estimates temporal consistency using p x i x o design in G-Theory
#' as recommended by DeSimone(2015). The input consistes of two data frames
#' containing the focal items of interest at time 1 (df1) and time 2 (df2).
#'
#' @param it1 Item scores at time 1
#' @param tot1 Total test score at time 1
#' @param it2 Item scores at time 2
#' @param tot2 Total test score at time 1
#' @return Estimates of item-level and test-level test-retest correlations
#' @export

cor_tc <- function(it1,tot1,it2,tot2) {
  test.level <- stats::cor(tot1,tot2,use = "pairwise.complete.obs")
  item.level <- diag(stats::cor(it1,it2, use = "pairwise.complete.obs"))
  results <- list(c(test.level=test.level), c(item.level=item.level))
  return(results)
}
