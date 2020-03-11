#' D Estimate for Personal Temporal Consistency
#'
#' This function estimates person-level temporal consistency
#' by estimating Mahalanobis D and D^2 statistics for personal
#' temporal consistency as recommended by DeSimone(2015).
#' The D^2 statistic is distributed as a Chi-Square with
#' degrees of freedom equal to the number of focal items.
#' The input consistes of the focal items measured at
#' time 1 (it1) and time 2 (it2).
#' @param it1 Columns in a dataframe or matrix containing the items measured at time 1
#' @param it2 Columns in a dataframe or matrix containing the items measured at time 2
#' @return D and D2 values for personal temporal consistency
#' @export
d_ptc <- function(it1,it2){
  step1 <- as.matrix(it1-it2) # matrix of differences in item scores between time 1 and time 2
  step2 <- t(step1) %*% step1 # transpose of matrix of differences multiplied by matrix of differences
  step3 <- step2 * (1/(nrow(it1)-1)) # multiplying resultant matrix by 1/N-1
  step4 <- solve(step3) # creating dif.xx matrix
  magicfor::magic_for(silent=T)
  for (subject in c(1:nrow(it1))) {
    D2 <- as.matrix(t(step1[subject,])) %*% step4 %*% as.matrix(step1[subject,])
    D <- sqrt(as.matrix(t(step1[subject,])) %*% step4 %*% as.matrix(step1[subject,]))
    magicfor::put(D,D2)
    }
  magicfor::magic_result_as_dataframe()
}










# years <- c(1:5)
# data <- data.frame()
# for(z in 1:length(years)){
#   year <- years[z]
#   for(i in 1:100){
#     value <- i+2+z
#     data <- rbind(data, cbind(year, value))
#   }
# }
# means <- data.frame()
# for(year in years){
#   yrmean <- mean(data$value[which(data$year==year)])
#   firstQ <- quantile(data$value[which(data$year==year)], c(0.25))
#   thirdQ <- quantile(data$value[which(data$year==year)], c(0.75))
#   means <- rbind(means, cbind(year,yrmean,firstQ,thirdQ))
# }



