#' D Estimate for Personal Temporal Consistency
#'
#' This function estimates person-level temporal consistency
#'   by estimating Mahalanobis D and D^2 statistics for personal
#'   temporal consistency as recommended by DeSimone(2015).
#'   The D^2 statistic is distributed as a Chi-Square with
#'   degrees of freedom equal to the number of focal items.
#'   The input consistes of the focal items measured at
#'   time 1 (it1) and time 2 (it2).
#'
#' Link to DeSimone(2015): https://journals.sagepub.com/doi/full/10.1177/1094428114553061
#'
#' @param id Columne in a dataframe or matrix containing the subject ID variable
#' @param it1 Columns in a dataframe or matrix containing the items measured at time 1
#' @param it2 Columns in a dataframe or matrix containing the items measured at time 2
#' @return D and D2 values for personal temporal consistency
#' @export
#'
#' @examples
#' data(desimone1)
#' names(desimone1)
#' d_ptc(desimone1[,1], desimone1[,c(2:6)],desimone1[,c(8:12)])

#'
d_ptc <- function(id, it1,it2){
  step1 <- as.matrix(it1-it2) # matrix of differences in item scores between time 1 and time 2
  step2 <- t(step1) %*% step1 # transpose of matrix of differences multiplied by matrix of differences
  step3 <- step2 * (1/(nrow(it1)-1)) # multiplying resultant matrix by 1/N-1
  step4 <- solve(step3) # creating dif.xx matrix
  output <- data.frame(matrix(nrow=nrow(it1),ncol=4))
  colnames(output) <- c("id","D","D2","pvalue")
    for (i in 1:nrow(it1)){
      subid <- id[i]
      D2 <- as.matrix(t(step1[i,])) %*% step4 %*% as.matrix(step1[i,])
      D <- sqrt(D2)
      pvalue <- stats::pchisq(D2,df=ncol(it1),lower.tail=FALSE)
      output[i,1] <- subid
      output[i,2] <- round(D,digits=2)
      output[i,3] <- round(D2,digits=2)
      output[i,4] <- round(pvalue,digits=3)
    }
  return(output)
  }
