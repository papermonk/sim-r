
computeTilesData <- function(n, df){
  
  num_ntile <- n
  lift <- df
  #sort decreasing by score
  lift <- lift[order(-lift[, 1]), ]
  
  #define ntiles(equal to the number of rows)
  ntile <- ceiling(seq_along(lift[,2])/floor(length(lift[,2])/num_ntile))
  lift2 <- cbind(lift, ntile)
  length(unique(ntile))
  
  #total scored
  cap <- floor(length(lift[, 2])/num_ntile) * num_ntile
  
  #total actuals caughts
  out <- aggregate(lift[1:cap, 2], by = list(ntile[1:cap]), sum)#total captured
  
  colnames(out) <- c("ntile","actuals")
  
  out$total <- floor(length(lift[, 2])/num_ntile) # ntile size
  
  out$ntile_precision <- out$actuals/out$total #precision
  
  overall_response_rate <- sum(out$actuals)/cap
  
  out$cumulative_precision <- cumsum(out$ntile_precision)/seq_along(out$ntile_precision) #cumsum
  out$cumulative_lift <- out$cumulative_precision/overall_response_rate
  out$recall <- cumsum(out$actuals)/sum(lift[,2])
  score_df <- sqldf::sqldf("select min(p1) as min_score, max(p1) as max_score, avg(p1) as average_score, ntile from lift2 group by ntile")
  score_df <- score_df[1:num_ntile,]
  out2 <- merge(out,score_df, by.x = "ntile", by.y = "ntile")
  return(out2)
}