#' D Estimate for Personal Temporal Consistency
#'
#' This function estimates person-level temporal consistency
#' by estimating Mahalanobis D and D^2 statistics for personal
#' temporal consistency as recommended by DeSimone(2015).
#' The D^2 statistic is distributed as a Chi-Square with
#' degrees of freedom equal to the number of focal items.
#' The input consistes of the focal items measured at
#' time 1 (it1) and time 2 (it2).
#' @param id Columne in a dataframe or matrix containing the subject ID variable
#' @param it1 Columns in a dataframe or matrix containing the items measured at time 1
#' @param it2 Columns in a dataframe or matrix containing the items measured at time 2
#' @return D and D2 values for personal temporal consistency
#' @export
d_ptc <- function(id, it1,it2){
  step1 <- as.matrix(it1-it2) # matrix of differences in item scores between time 1 and time 2
  step2 <- t(step1) %*% step1 # transpose of matrix of differences multiplied by matrix of differences
  step3 <- step2 * (1/(nrow(it1)-1)) # multiplying resultant matrix by 1/N-1
  step4 <- solve(step3) # creating dif.xx matrix
  output <- data.frame(matrix(nrow=nrow(it1),ncol=3))
  colnames(output) <- c("id","D","D2")
    for (i in 1:nrow(it1)){
      subid <- id[i]
      D2 <- as.matrix(t(step1[i,])) %*% step4 %*% as.matrix(step1[i,])
      D <- sqrt(D2)
      output[i,1] <- subid
      output[i,2] <- D
      output[i,3] <- D2
    }
  return(output)
  }
