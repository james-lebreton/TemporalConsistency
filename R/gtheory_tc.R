#' G-Theory Estimate for Temporal Consistency
#'
#' This function estimates temporal consistency using p x i x o design in G-Theory
#' as recommended by DeSimone(2015). The input consistes of two data frames
#' containing the focal items of interest at time 1 (df1) and time 2 (df2).
#'
#' @param df1 A dataframe or matrix containing the items measured at time 1
#' @param df2 A dataframe or matrix containing the items measured at time 2
#' @return Estimate of SRMR for item-level test-retest correlations
#' @export


#convert wide data to long form data
# df <- desimone2
# dflong <- df %>%
#   tidyr::pivot_longer(
#     cols = c(2:6,8:12),
#     names_to = c("item", "time"),
#     values_to = "score",
#     names_pattern = "i?(.)(.)")
# dflong
# summary(output<-(aov(score~id*item*time,data=dflong)))
# formula2 <-
#   score~(1|id)+(1|item)+
#   (1|time)+
#   (1|id:item)+(1|id:time)+
#   (1|time:item)
# g2 <- gtheory::gstudy(data=dflong,formula2);g2
#
#
# #gtheory_tc <- function(id,item,time,score) {
#
# #}
